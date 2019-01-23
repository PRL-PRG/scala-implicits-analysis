import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.io.*;

public class HashesToInts {

    public static void help() {
        System.out.println("h2i FOLDER");
        System.out.println("    Converts hashes in files and stats tokenizer outputs to integers");
        System.out.println("    FOLDER - folder where to look for the txt files");
        System.out.println("");
    }

    public static void convert(String[] args) {
        if (args.length < 2)
            throw new RuntimeException("Invalid number of arguments");
        String folder = args[1];
        HashesToInts h = new HashesToInts(folder);
        h.convertFiles();
        h.convertStats();
    }

    private HashesToInts(String folder) {
        folder_ = folder;
        fileHashes_ = new HashMap<String, Long>();
        tokenHashes_ = new HashMap<String, Long>();
    }

    private long convertFileHash(String hash) {
        if (fileHashes_.containsKey(hash)) {
            return fileHashes_.get(hash);
        } else {
            long size = fileHashes_.size();
            fileHashes_.put(hash, size);
            return size;
        }
    }

    private long convertTokenHash(String hash) {
        if (tokenHashes_.containsKey(hash)) {
            return tokenHashes_.get(hash);
        } else {
            long size = tokenHashes_.size();
            tokenHashes_.put(hash, size);
            return size;
        }
    }

    private void convertFiles() {
        String filename = folder_ + "/" + Config.FILES + ".csv";
        System.out.println("Rewriting file hashes...");
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename + ".h2i"), "utf-8"))) {
            int total = CSVReader.file(filename, (ArrayList<String> row) -> {
                row.set(3, String.valueOf(convertFileHash(row.get(3))));
                // now output the file
                Helpers.writeFilesRow(row, writer);
            });
            System.out.println("    total rows:          " + total);
            System.out.println("    unique file hashes:  " + fileHashes_.size());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    private void convertStats() {
        String filename = folder_ + "/" + Config.STATS + ".csv";
        System.out.println("Rewriting stats (file and token hashes)...");
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename + ".h2i"), "utf-8"))) {
            int total = CSVReader.file(filename, (ArrayList<String> row) -> {
                row.set(0, String.valueOf(convertFileHash(row.get(0))));
                row.set(7, String.valueOf(convertTokenHash(row.get(7))));
                // now output the file
                Helpers.writeRow(row, writer);
            });
            System.out.println("    total rows:          " + total);
            System.out.println("    unique file hashes:  " + fileHashes_.size());
            System.out.println("    unique token hashes: " + fileHashes_.size());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private String folder_;

    private HashMap<String, Long> fileHashes_;
    private HashMap<String, Long> tokenHashes_;
}
