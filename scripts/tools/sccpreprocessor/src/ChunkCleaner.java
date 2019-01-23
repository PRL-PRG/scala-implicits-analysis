import java.io.*;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

/** Cleans the specified tokenizer input so that only chunks from min to max will be kept.
 *
 * Starts by reading the tokenized file and discards any fileIDs that do not fit the <min,max> interval modulo number of chunks. For each file it keeps it remembers its file hash in set, and for each file it discards it remembers its project as these projects must be discarded as well.
 */
public class ChunkCleaner {
    public static void help() {
        System.out.println("clean NUM_CHUNKS MIN MAX FOLDER [SUFFIX]");
        System.out.println("    Cleans the projects, files, and stats tables and their derivatives so that they only contain data from chunks >=MIN and <= MAX.");
        System.out.println("    NUM_CHUNKS - total number of chunks in the input (as given to the tokenizer)");
        System.out.println("    MIN - min chunk index to be included in the clean output");
        System.out.println("    MAX - max chunk index to be included in the clean output");
        System.out.println("    FOLDER - folder where to look for the CSV files");
        System.out.println("    SUFFIX - optional argument for suffix of the clean files. If not specified it is set to '.clean-num-min-max'");
        System.out.println("");
    }


    /* Usage: clean NUM_CHUNKS MIN_CHUNK MAX_CHUNK FOLDER [SUFFIX]
     */
    public static void clean(String [] args) {
        if (args.length < 5)
            throw new RuntimeException("Invalid number of arguments");
        int numChunks = Integer.parseInt(args[1]);
        int minChunk = Integer.parseInt(args[2]);
        int maxChunk = Integer.parseInt(args[3]);
        String folder = args[4];
        String suffix;
        if (args.length == 6)
            suffix = args[5];
        else
            suffix = ".clean-" + numChunks + "-" + minChunk + "-" + maxChunk;
        ChunkCleaner c = new ChunkCleaner(numChunks, minChunk, maxChunk, folder, suffix);
        c.cleanFiles();
        c.cleanFilesExtra();
        c.cleanProjects();
        c.cleanProjectsExtra();
        c.cleanGenericStats();
        c.cleanJsStats();
    }

    private ChunkCleaner(int numChunks, int minChunk, int maxChunk, String folder, String suffix) {
        numChunks_ = numChunks;
        minChunk_ = minChunk;
        maxChunk_ = maxChunk;
        folder_ = folder;
        cleanSuffix_ = suffix;
        validFiles_ = new HashSet<String>();
        invalidProjects_ = new HashSet<Integer>();
    }

    private void cleanFiles() {
        String filename = folder_ +"/" + Config.FILES + ".csv";
        System.out.println("Cleaning files...");
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename + cleanSuffix_), "utf-8"))) {
            deletedCounter_ = 0;
            int total = CSVReader.file(filename, (ArrayList<String> row) -> {
                int id = Integer.parseInt(row.get(0));
                if (id % numChunks_ < minChunk_ || id % numChunks_ > maxChunk_) {
                    invalidProjects_.add(Integer.parseInt(row.get(1))); // if the file is not to make it to the
                    ++deletedCounter_;
                } else {
                    Helpers.writeFilesRow(row, writer);
                    validFiles_.add(row.get(3)); // add the hash to valid hashes
                }
            });
            System.out.println("    total rows:        " + total);
            System.out.println("    ignored rows:      " + deletedCounter_);
            System.out.println("    kept rows:         " + (total - deletedCounter_));
            System.out.println("    invalid projects:  " + invalidProjects_.size());
            System.out.println("    valid file hashes: " + validFiles_.size());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void cleanFilesExtra() {
        String filename = folder_ +"/" + Config.FILES_EXTRA + ".csv";
        System.out.println("Cleaning files extra information...");
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename + cleanSuffix_), "utf-8"))) {
            deletedCounter_ = 0;
            int total = CSVReader.file(filename, (ArrayList<String> row) -> {
                int id = Integer.parseInt(row.get(0));
                if (id % numChunks_ < minChunk_ || id % numChunks_ > maxChunk_) {
                    ++deletedCounter_;
                } else {
                    Helpers.writeRow(row, writer);
                }
            });
            System.out.println("    total rows:        " + total);
            System.out.println("    ignored rows:      " + deletedCounter_);
            System.out.println("    kept rows:         " + (total - deletedCounter_));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void cleanProjects() {
        String filename = folder_ +"/" + Config.PROJECTS + ".csv";
        System.out.println("Cleaning projects information...");
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename + cleanSuffix_), "utf-8"))) {
            deletedCounter_ = 0;
            int total = CSVReader.file(filename, (ArrayList<String> row) -> {
                int id = Integer.parseInt(row.get(0));
                if (invalidProjects_.contains(id)) {
                    ++deletedCounter_;
                } else {
                    Helpers.writeRow(row, writer);
                }
            });
            System.out.println("    total rows:        " + total);
            System.out.println("    ignored rows:      " + deletedCounter_);
            System.out.println("    kept rows:         " + (total - deletedCounter_));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void cleanProjectsExtra() {
        String filename = folder_ +"/" + Config.PROJECTS_EXTRA + ".csv";
        System.out.println("Cleaning projects extra information...");
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename + cleanSuffix_), "utf-8"))) {
            deletedCounter_ = 0;
            int total = CSVReader.file(filename, (ArrayList<String> row) -> {
                int id = Integer.parseInt(row.get(0));
                if (invalidProjects_.contains(id)) {
                    ++deletedCounter_;
                } else {
                    Helpers.writeRow(row, writer);
                }
            });
            System.out.println("    total rows:        " + total);
            System.out.println("    ignored rows:      " + deletedCounter_);
            System.out.println("    kept rows:         " + (total - deletedCounter_));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void cleanGenericStats() {
        String filename = folder_ +"/" + Config.GENERIC_STATS + ".csv";
        System.out.println("Cleaning generic statistics information...");
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename + cleanSuffix_), "utf-8"))) {
            deletedCounter_ = 0;
            int total = CSVReader.file(filename, (ArrayList<String> row) -> {
                String fileHash = row.get(0);
                if (validFiles_.contains(fileHash)) {
                    Helpers.writeRow(row, writer);
                } else {
                    ++deletedCounter_;
                }
            });
            System.out.println("    total rows:        " + total);
            System.out.println("    ignored rows:      " + deletedCounter_);
            System.out.println("    kept rows:         " + (total - deletedCounter_));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void cleanJsStats() {
        String filename = folder_ +"/" + Config.JS_STATS + ".csv";
        System.out.println("Cleaning javasript statistics information...");
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename + cleanSuffix_), "utf-8"))) {
            deletedCounter_ = 0;
            int total = CSVReader.file(filename, (ArrayList<String> row) -> {
                String fileHash = row.get(0);
                if (validFiles_.contains(fileHash)) {
                    Helpers.writeRow(row, writer);
                } else {
                    ++deletedCounter_;
                }
            });
            System.out.println("    total rows:        " + total);
            System.out.println("    ignored rows:      " + deletedCounter_);
            System.out.println("    kept rows:         " + (total - deletedCounter_));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private String folder_;
    private String cleanSuffix_;
    private int minChunk_;
    private int maxChunk_;
    private int numChunks_;
    private Set<Integer> invalidProjects_;
    private Set<String> validFiles_;

    private int deletedCounter_;

}
