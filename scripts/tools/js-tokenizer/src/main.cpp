#ifndef __x86_64__
#error "Must use 64 bit system!"
#endif

#include <cstring>
#include <iomanip>
#include <thread>

#include "workers/CSVReader.h"

std::string CSV_file = "";
bool quiet = false;

void tokenize();
void mergeResults();

void addTokenizer(TokenizerKind k) {
    Tokenizer::AddTokenizer(k);
    Merger::AddTokenizer(k);
}

void loadDefaults() {

    Writer::SetQueueMaxLength(1000);
    TokenizedFile::SetMaxInstances(1000000);
    ClonedProject::SetMaxInstances(100000);

    ClonedProject::StrideCount() = 1;
    ClonedProject::StrideIndex() = -1;

    CSVReader::SetLanguage("JavaScript");

    Downloader::DownloadDir() = "download";
    Writer::OutputDir() = "";
    ClonedProject::KeepProjects() = true;

    addTokenizer(TokenizerKind::Generic);
}

// language input file, num strides, stride, output dir
void setup(int argc, char * argv[]) {
    if (argc != 6 && argc != 7 )
        throw STR("Invalid number of arguments");
    if (argc == 7 && strncmp(argv[6],"quiet", 6) == 0)
        quiet = true;
    loadDefaults();
    CSVReader::SetLanguage(argv[1]);
    CSV_file = argv[2];
    ClonedProject::StrideCount() = std::atoi(argv[3]);
    ClonedProject::StrideIndex() = std::atoi(argv[4]);
    // set the stride id for the files so that they are unique across strides
    TokenizedFile::InitializeStrideId();
    Writer::OutputDir() = argv[5];

    Thread::InitializeLog(STR(Writer::OutputDir() << "/log-" << ClonedProject::StrideIndex() << ".txt"));
}










/** Usage:

  tokenizer COUNT INDEX db_name CSV_file

 */
int main(int argc, char * argv[]) {
    try {
        setup(argc, argv);
        tokenize();
    } catch (std::string const & e) {
        std::cerr << e << std::endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
