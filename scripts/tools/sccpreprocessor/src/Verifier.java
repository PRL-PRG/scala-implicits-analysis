import java.io.*;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

/** Verifies the results of the sourcerer CC input wrt to the stats table.
 */
public class Verifier {

    public static void help() {
        System.out.println("verify FOLDER");
        System.out.println("    Verifies the tokenization results in given folder.");
        System.out.println("    FOLDER - folder where to look for the CSV files");
        System.out.println("");
    }


    /* Usage: clean NUM_CHUNKS MIN_CHUNK MAX_CHUNK FOLDER [SUFFIX]
     */
    public static void verify(String [] args) throws IOException {
        if (args.length < 2)
            throw new RuntimeException("Invalid number of arguments");
        String folder = args[1];
        String suffix = "";
        if (args.length == 3)
            suffix = args[2];
        Verifier v = new Verifier(folder, suffix);
        //v.loadStatsTokenHashes("generic");
        //v.checkTokenizedFiles("generic");
        v.loadFileHashes();
        v.checkFileHashes("generic");
    }


    private Verifier(String folder, String suffix) {
        folder_ = folder;
        suffix_ = suffix;
    }

    /** Loads all unique token hashes in the given stats file.
     */
    private void loadStatsTokenHashes(String tokenizer) {
        hashes_ = new HashSet<String>();
        String filename = folder_ + "/" + tokenizer + "/" + Config.STATS + suffix_ + ".txt";
        System.out.println("Reading unique token hashes from " + filename);
        int total = CSVReader.file(filename, (ArrayList<String> row) -> {
            String hash = row.get(7);
            hashes_.add(hash);
        });
        System.out.println("    total rows:         " + total);
        System.out.println("    unique token hashes:" + hashes_.size());
    }

    /** Loads file hashes from files table.
     */
    private void loadFileHashes() {
        hashes_ = new HashSet<String>();
        String filename = folder_ + "/" + Config.FILES + suffix_ + ".txt";
        System.out.println("Reading unique file hashes from " + filename);
        int total = CSVReader.file(filename, (ArrayList<String> row) -> {
            String hash = row.get(3);
            hashes_.add(hash);
        });
        System.out.println("    total rows:         " + total);
        System.out.println("    unique token hashes:" + hashes_.size());
    }

    private void checkFileHashes(String tokenizer) throws IOException {
        seenHashes_ = new HashSet<String>();
        duplicateHashes_ = new HashSet<String>();
        unknownHashes_ = new HashSet<String>();
        String filename = folder_ + "/" + tokenizer + "/" + Config.STATS + suffix_ + ".txt";
        System.out.println("Reading unique token hashes from " + filename);
        int total = CSVReader.file(filename, (ArrayList<String> row) -> {
            String hash = row.get(0);
            if (hashes_.contains(hash)) {
                hashes_.remove(hash);
                seenHashes_.add(hash);
            } else if (seenHashes_.contains(hash)) {
                duplicateHashes_.add(hash);
            } else {
                unknownHashes_.add(hash);
            }
        });
        System.out.println("    total rows:          " + total);
        System.out.println("    valid hashes:        " + seenHashes_.size());
        System.out.println("    duplicate hashes:    " + duplicateHashes_.size());
        System.out.println("    hashes not in stats: " + hashes_.size());
        System.out.println("    hashes not in files: " + unknownHashes_.size());

        if (hashes_.size() > 0) {
            try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder_ + "/" + tokenizer + "/missingFromStats" + suffix_ + ".txt"), "utf-8"))) {
                for (String s : hashes_)
                    writer.write(s + "\n");
            }
        }
        if (duplicateHashes_.size() > 0) {
            try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder_ + "/" + tokenizer + "/fileHashDuplicatesInStats" + suffix_ + ".txt"), "utf-8"))) {
                for (String s : duplicateHashes_)
                    writer.write(s + "\n");
            }
        }
        if (unknownHashes_.size() > 0) {
            try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder_ + "/" + tokenizer + "/missingFromFiles" + suffix_ + ".txt"), "utf-8"))) {
                for (String s : unknownHashes_)
                    writer.write(s + "\n");
            }
        }
    }

    private void checkTokenizedFiles(String tokenizer) throws IOException {
        seenHashes_ = new HashSet<String>();
        duplicateHashes_ = new HashSet<String>();
        unknownHashes_ = new HashSet<String>();
        String filename = folder_ + "/" + tokenizer + "/" + Config.TOKENIZED_FILES + suffix_ + ".txt";
        System.out.println("Crossreferencing against tokenized files " + filename);
        int total = LineReader.file(filename, (String line) -> {
            String hash = line.split(",", 6)[4];
            if (hash.charAt(0) == '"')
                hash = hash.substring(1, hash.length() - 1);
            if (seenHashes_.contains(hash))
                duplicateHashes_.add(hash);
            else if (hashes_.contains(hash)) {
                hashes_.remove(hash);
                seenHashes_.add(hash);
            } else {
                unknownHashes_.add(hash);
            }
        });
        System.out.println("    total rows:                     " + total);
        System.out.println("    hashes in stats, not in tokens: " + hashes_.size());
        System.out.println("    valid hashes:                   " + seenHashes_.size());
        System.out.println("    duplicate hashes:               " + duplicateHashes_.size());
        System.out.println("    hashes not in stats:            " + unknownHashes_.size());
/*        if (hashes_.size() > 0) {
            try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder_ + "/" + tokenizer + "/missingFromTokens.csv"), "utf-8"))) {
                for (String s : hashes_)
                    writer.write(s + "\n");
            }
        }
        if (duplicateHashes_.size() > 0) {
            try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder_ + "/duplicatesInStats.csv"), "utf-8"))) {
                for (String s : duplicateHashes_)
                    writer.write(s + "\n");
            }
        }
        if (unknownHashes_.size() > 0) {
            try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder_ + "/missingFromStats.csv"), "utf-8"))) {
                for (String s : unknownHashes_)
                    writer.write(s + "\n");
            }
        } */
    }

    String folder_;
    String suffix_;
    Set<String> hashes_;
    Set<String> seenHashes_;
    Set<String> duplicateHashes_;
    Set<String> unknownHashes_;

}
