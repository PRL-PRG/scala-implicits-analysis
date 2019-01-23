import java.io.*;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

/** SourcererCC requires tokenized files to be sorted according to their id in the input. This script accomplishes it.
 */
public class TokenizedFilesSorter {

    public static void help() {
        System.out.println("sort FOLDER [SUFFIX]");
        System.out.println("    Sort tokenized_files.csv in given folder so that it can be passed to sourcererCC. ");
        System.out.println("    FOLDER - folder where to look for the CSV files");
        System.out.println("    SUFFIX - optional argument for suffix of the sorted files. If not specified it is set to '.sorted'");
        System.out.println("");
    }

    /** Usage: sort FOLDER [SUFFIX]
     */
    public static void sort(String[] args) {
        if (args.length < 2)
            throw new RuntimeException("Invalid number of arguments");
        String folder = args[1];
        String suffix;
        if (args.length == 3)
            suffix = args[2];
        else
            suffix = ".sorted";
        TokenizedFilesSorter s = new TokenizedFilesSorter(folder, suffix);
        s.sortTokenizedFiles();
    }

    private TokenizedFilesSorter(String folder, String suffix) {
        folder_ = folder;
        suffix_ = suffix;
        tokenizedFiles_ = new TreeMap<Integer, String>();
    }

    private void sortTokenizedFiles() {
        String filename = folder_ +"/" + Config.TOKENIZED_FILES + ".csv";
        System.out.println("Sorting tokenized files...");
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename + suffix_), "utf-8"))) {
            int total = LineReader.file(filename, (String line) -> {
                String[] parts = line.split(",",3);
                int id = Integer.parseInt(parts[1]);
                tokenizedFiles_.put(id, parts[0] + "," + parts[2]);
            });
            System.out.println("    total rows:        " + total);
            System.out.println("  writing...");
            for (Map.Entry<Integer, String> entry : tokenizedFiles_.entrySet()) {
                String[] parts = entry.getValue().split(",", 2);
                writer.write(parts[0]);
                writer.write(",");
                writer.write(String.valueOf(entry.getKey()));
                writer.write(",");
                writer.write(parts[1]);
                writer.write("\n");
            }
            System.out.println("    done.");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private String folder_;
    private String suffix_;


    private Map<Integer, String> tokenizedFiles_;



}
