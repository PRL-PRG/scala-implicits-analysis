import java.io.*;
import java.util.ArrayList;

/** Rewrites IDs in given input so that they correspond to id's from given chunk id.

 Does so in a stupid way by just adding an extra mod N chunks at the end of each id. We only need to rewrite files as project ids are the same, as well as file hashes.

 However, tokenized_files need to be rewritten as well. For this the script assumes that there is no stride information at the end of the files.
 */
public class ChunkRewriter {

    public static void help() {
        System.out.println("rewrite NUM_CHUNKS ID FOLDER [SUFFIX]");
        System.out.println("    Rewrites file indices in given output so that they correspon to selected stride (specified by number of chunks and stride index).");
        System.out.println("    NUM_CHUNKS - total number of chunks expected in input");
        System.out.println("    ID - rewritten ID for file indices");
        System.out.println("    FOLDER - folder where to look for the CSV files");
        System.out.println("    SUFFIX - optional argument for suffix of the rewritten files. If not specified it is set to '.rewritten-id'");
        System.out.println("");
    }

    /* Usage: rewrite NUM_CHUNKS CHUNK_ID FOLDER [SUFFIX]
     */
    public static void rewrite(String[] args) {
        if (args.length < 4)
            throw new RuntimeException("Invalid number of arguments");
        int numChunks = Integer.parseInt(args[1]);
        int chunkId = Integer.parseInt(args[2]);
        String folder = args[3];
        String suffix;
        if (args.length == 5)
            suffix = args[4];
        else
            suffix = ".rewritten-" + chunkId;
        ChunkRewriter r = new ChunkRewriter(numChunks, chunkId, folder, suffix);
        r.rewriteFiles();
        r.rewriteFilesExtra();
        r.rewriteTokenizedFiles("generic");
        r.rewriteTokenizedFiles("js");
    }

    private ChunkRewriter(int numChunks, int chunkId, String folder, String suffix) {
        numChunks_ = numChunks;
        chunkId_ = chunkId;
        folder_ = folder;
        suffix_ = suffix;
    }

    private void rewriteFiles() {
        String filename = folder_ +"/" + Config.FILES + ".csv";
        System.out.println("Rewriting files...");
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename + suffix_), "utf-8"))) {
            int total = CSVReader.file(filename, (ArrayList<String> row) -> {
                long id = Long.parseLong(row.get(0));
                // if the id is not from proper chunk, change it
                if (id % numChunks_ != chunkId_) {
                    id = id * numChunks_ + chunkId_;
                    row.set(0, String.valueOf(id));
                }
                // now output the file
                Helpers.writeFilesRow(row, writer);
            });
            System.out.println("    total rows:        " + total);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void rewriteFilesExtra() {
        String filename = folder_ +"/" + Config.FILES_EXTRA + ".csv";
        System.out.println("Rewriting files extra...");
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename + suffix_), "utf-8"))) {
            int total = CSVReader.file(filename, (ArrayList<String> row) -> {
                long id = Long.parseLong(row.get(0));
                // if the id is not from proper chunk, change it
                if (id % numChunks_ != chunkId_) {
                    id = id * numChunks_ + chunkId_;
                    row.set(0, String.valueOf(id));
                }
                // now output the file
                Helpers.writeRow(row, writer);
            });
            System.out.println("    total rows:        " + total);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void rewriteTokenizedFiles(String tokenizer) {
        String filename = folder_ +"/" + tokenizer +"/" + Config.TOKENIZED_FILES + ".csv";
        System.out.println("Rewriting tokenized files (" + tokenizer + ")...");
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename + suffix_), "utf-8"))) {
            int total = LineReader.file(filename, (String line) -> {
                String[] parts = line.split(",",3);
                long id = Long.parseLong(parts[1]);
                if (id % numChunks_ != chunkId_)
                    id = id * numChunks_ + chunkId_;
                writer.write(parts[0]);
                writer.write(",");
                writer.write(String.valueOf(id));
                writer.write(",");
                writer.write(parts[2]);
                writer.write("\n");
            });
            System.out.println("    total rows:        " + total);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private String folder_;
    private String suffix_;
    private int numChunks_;
    private int chunkId_;
}
