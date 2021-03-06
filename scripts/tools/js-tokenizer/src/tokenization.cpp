#include <iomanip>
#include <thread>
#include <signal.h>
#include <unistd.h>

#include "data.h"
#include "buffers.h"
#include "worker.h"

#include "workers/CSVReader.h"


extern std::string CSV_file;
extern bool quiet;

// reporting

template<typename T>
std::string reportLivingObjects(std::string const & name) {
    return STR("  " << std::setw(50) << std::left << name  <<
        std::setw(12) << std::right << T::Instances() <<
        std::setw(4) << pct(T::Instances(), T::MaxInstances()) << std::endl);
}

template<typename T>
std::string reportTokenizerFileStats(unsigned totalFiles) {
    unsigned errors = T::ErrorFiles();
    unsigned fileHashes = Merger::UniqueFileHashes(T::kind);
    unsigned tokenHashes = Merger::UniqueTokenHashes(T::kind);

    std::stringstream ss;
    ss << "  " << T::kind << std::endl;
    ss << "    Errors                                         " << std::setw(14) << errors << std::setw(4) << pct(errors, totalFiles) << std::endl;
    ss << "    Unique file hashes                             " << std::setw(14) << fileHashes << std::setw(4) /*<< pct(fileHashes, totalFiles) */ << std::endl;
    ss << "    Unique token hashes                            " << std::setw(14) << tokenHashes << std::setw(4) << pct(tokenHashes, totalFiles) << std::endl;
    return ss.str();
}

/** Waits for a second, then displays statistics. Returns true if all jobs have been finished and all threads are idle, false otherwise. */
bool stats(std::string & output, std::chrono::high_resolution_clock::time_point const & since) {
    std::stringstream ss;


    // sleep for a second - this is not super precise but at the time it takes to run we do not care about such precission anyways...
    std::this_thread::sleep_for(std::chrono::seconds(1));
    auto now = std::chrono::high_resolution_clock::now();
    double secondsSince = std::chrono::duration_cast<std::chrono::milliseconds>(now - since).count() / 1000.0;

    // get all statistics
    std::vector<Thread::Stats> s;
    s.push_back(CSVReader::Statistics());
    s.push_back(Downloader::Statistics());
    s.push_back(Tokenizer::Statistics());
    s.push_back(Merger::Statistics());
    s.push_back(Writer::Statistics());

    // get other info
    unsigned long projects = s[2].jobsDone;
    unsigned long files = Tokenizer::TotalFiles();
    unsigned long bytes = Tokenizer::TotalBytes();

    // a very simple heuristic where we have assumed projects to have
    unsigned EXPECTED_PROJECTS = s[1].queueSize + s[1].jobsDone;
    if (EXPECTED_PROJECTS < 2300000)
        EXPECTED_PROJECTS = 2300000;

    // adjust for the stride calculation
    EXPECTED_PROJECTS /= ClonedProject::StrideCount();

    unsigned memory = 0;
    memory += sizeof(ClonedProject) * ClonedProject::Instances();
    memory += sizeof(TokenizedFile) * TokenizedFile::Instances();
    memory += sizeof(TokensMap) * TokensMap::Instances();
    memory += sizeof(CloneGroup) * Merger::NumCloneGroups();
    memory += sizeof(CloneGroup) * Merger::NumTokens();
    memory += sizeof(Hash) * Merger::UniqueFileHashes();
    memory += sizeof(std::string) * s[0].queueSize;
    memory += sizeof(DownloaderJob) * s[1].queueSize;
    memory += sizeof(TokenizerJob) * s[2].queueSize;
    memory += sizeof(MergerJob) * s[3].queueSize;
    memory += (1024 * 1024) * s[4].queueSize; // these are large buffers
    memory += sizeof(WriterJob) * s[5].queueSize;

    // add downloader errors to the # of projects
    double secondsTotal = secondsSince * EXPECTED_PROJECTS / (s[1].errors + projects);
    // Worker statistics
    ss << "Worker                      Up   I*   %  S*    %  QSize       Done   Err Fatal   %" << std::endl;
    ss << "-------------------------- --- ---  --- ---  --- ------ ---------- ----- ----- ---" << std::endl;
    for (Thread::Stats & stats : s)
        //if (stats.started > 0)
            ss << "  " << stats << std::endl;
    ss << std::endl;

    ss << "Statistics                                                     #   %" << std::endl;
    ss << "--------------------------------------------------- ------------ ---" << std::endl;
    ss << "  Elapsed time                                     " << std::setw(14) << time(secondsSince) << std::setw(4) << pct(secondsSince, secondsTotal) << std::endl;
    ss << "    Estimated remaining time                       " << std::setw(14) << time(secondsTotal - secondsSince) << std::endl;
    ss << "  Projects                                         " << std::setw(14) << projects << std::setw(4) << pct(Tokenizer::JobsDone(), EXPECTED_PROJECTS) << std::endl;
    ss << "  Files                                            " << std::setw(14) << files << std::endl;
    ss << reportTokenizerFileStats<GenericTokenizer>(files);


    // general progress information
    ss << "Speed                                                              /s" << std::endl;
    ss << "------------------------------------------------ ---------- ---------" << std::endl;
    ss << "  Files                                         " << std::setw(11) << files << std::setw(10) << round(files / secondsSince, 2) << std::endl;
    ss << "  Bytes                                         " << std::setw(11) <<xbytes(bytes) << std::setw(10) << xbytes(bytes / secondsSince) << std::endl;
    ss << std::endl;


    // Memory statistics
    ss << "Living objects                                                 #   %" <<  std::endl;
    ss << "--------------------------------------------------- ------------ ---" << std::endl;
    ss << reportLivingObjects<ClonedProject>("Projects");
    ss << reportLivingObjects<TokenizedFile>("Files");
    ss << reportLivingObjects<TokensMap>("Token Maps");
//    ss << "  Unique file hashes                                " << std::setw(12) << Merger::UniqueFileHashes() << std::endl;
    ss << "  Generic" << std::endl;
    ss << "    Clone groups objects                            " << std::setw(12) << Merger::NumCloneGroups(TokenizerKind::Generic) << std::endl;
    ss << "    Token info objects                              " << std::setw(12) << Merger::NumTokens(TokenizerKind::Generic) << std::endl;
    ss << std::endl;

    output = ss.str();

    for (Thread::Stats & stats : s)
        if (stats.queueSize > 0 or stats.idle != stats.started)
            return false;
    return true;
}


// setup & teardown

template<typename T>
void initializeThreads(unsigned num) {
    Thread::InitializeWorkers<T>(num);
    Thread::Print(STR("    " <<  T::Name() << " (" << num << ")" << std::endl));
}

/** Resumes state so that we do not have to INSERT IGNORE into the stats tables. To be on a safe side, only ingests unique file hashes for both tokenizers if there are any in the tables.

  TODO note that this assumes that we are running both tokenizers, which we indeed are.
 */
void resumeState() {

    /*
    if (ClonedProject::StrideIndex() == 0)
        return; // nothing to resume

    Thread::Print(STR("Resuming previous state" << std::endl));
    DBWriter::CheckDatabase();
    SQLConnection sql;
    sql.query(STR("USE " << DBWriter::DatabaseName()));
    std::string tableName = Buffer::TableName(Buffer::Kind::Stats, TokenizerKind::Generic, STR(ClonedProject::StrideIndex()));

    unsigned count = 0;
    sql.query(STR("SELECT fileHash FROM " << tableName), [& count] (unsigned cols, char ** row) {
       if (++count % 1000000 == 0)
           Thread::Print(STR("      " << count << std::endl), false);
       Hash h = Hash::Parse(row[0]);
       Merger::AddUniqueFileHash(TokenizerKind::Generic, h);
       Merger::AddUniqueFileHash(TokenizerKind::JavaScript, h);
    });
    Thread::Print(STR("      total: " <<  count << std::endl));
    */
}


void stampAndSummary(std::chrono::high_resolution_clock::time_point const & since) {
    auto now = std::chrono::high_resolution_clock::now();
    unsigned secondsSince = std::chrono::duration_cast<std::chrono::seconds>(now - since).count();
    Buffer & stamp = Buffer::Get(Buffer::Kind::Stamp);
    Buffer & summary = Buffer::Get(Buffer::Kind::Summary);

    stamp.append(STR("'last-stride-index'," << ClonedProject::StrideIndex()));

    summary.append(STR("'stride-index'," << ClonedProject::StrideIndex()));
    summary.append(STR("'stride-count'," << ClonedProject::StrideCount()));
    summary.append(STR("'time'," << secondsSince));
    summary.append(STR("'projects'," << Downloader::JobsDone() - Downloader::Errors()));
    summary.append(STR("'files'," << Tokenizer::TotalFiles()));
    summary.append(STR("'generic-files-unique'," << Merger::UniqueFileHashes(TokenizerKind::Generic)));
    summary.append(STR("'generic-files-tokens-unique'," << Merger::UniqueTokenHashes(TokenizerKind::Generic)));
    summary.append(STR("'csv-reader-jobs'," << CSVReader::JobsDone()));
    summary.append(STR("'csv-reader-errors'," << CSVReader::Errors()));
    summary.append(STR("'downloader-jobs'," << Downloader::JobsDone()));
    summary.append(STR("'downloader-errors'," << Downloader::Errors()));
    summary.append(STR("'tokenizer-jobs'," << Tokenizer::JobsDone()));
    summary.append(STR("'tokenizer-errors'," << Tokenizer::Errors()));
    summary.append(STR("'merger-jobs'," << Merger::JobsDone()));
    summary.append(STR("'merger-errors'," << Merger::Errors()));
    summary.append(STR("'file-writer-jobs'," << Writer::JobsDone()));
    summary.append(STR("'file-writer-errors'," << Writer::Errors()));
    Buffer::FlushAll();
}

// actual code

void tokenize() {
    Thread::Print(STR("Running strided tokenizer, stride N = " << ClonedProject::StrideCount() << ", index = " << ClonedProject::StrideIndex() << std::endl));
    // resume previous state, if any
    resumeState();
    auto start = std::chrono::high_resolution_clock::now();

    Thread::Print(STR("  initializing threads" << std::endl));
    initializeThreads<CSVReader>(1);
    initializeThreads<Downloader>(20);
    initializeThreads<Tokenizer>(20);
    initializeThreads<Merger>(1);
    initializeThreads<Writer>(1);
    Thread::Print(STR("  scheduling csv file " << CSV_file << std::endl));
    CSVReader::Schedule(CSV_file);

    // process all projects within current stride
    Thread::Print(STR("  processing..." << std::endl));
    std::string statsOutput;
    while (not stats(statsOutput, start)) {
        if (!quiet)
            Thread::Print(STR(CSI << "[J" << statsOutput << CSI << "[32A"), false);
    }

    // flush database buffers and store state to the database
    Thread::Print(STR("  flushing db buffers and storing the state..." << std::endl));

    std::atomic_int x(1);

    std::thread t([&x]() {
        try {
            Merger::FlushStatistics();
            Buffer::FlushAll();
            x = 0;
        } catch (std::string const & e) {
            Thread::Error(e);
        } catch (...) {
            Thread::Error("Unknown error");
        }
    });
    t.detach();

    while ((x == 1) or not stats(statsOutput, start)) {
        if (!quiet)
            Thread::Print(STR(CSI << "[J" << statsOutput << CSI << "[32A"), false);
    }
    Thread::Print(STR("  writing stamp..." << std::endl));
    stampAndSummary(start);

    while (not stats(statsOutput, start)) {
        if (!quiet)
            Thread::Print(STR(CSI << "[J" << statsOutput << CSI << "[32A"), false);
    }
    // all is done
    Thread::Print(statsOutput); // print last stats into the logfile as well
    if (not ClonedProject::KeepProjects()) {
        Thread::Print(STR("  deleting remaining projects..." << std::endl));
        exec("rm -rf", Downloader::DownloadDir());
    }
    Writer::Finalize();
    Thread::Print(STR("ALL DONE" << std::endl));
    //std::exit(0);
    //std::abort();
    kill(getpid(), SIGKILL);
}
