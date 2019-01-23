import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Created by peta on 24.7.17.
 */
public class CloneStats {

    public static void analyze(String[] args) {
        if (args.length < 3)
            throw new RuntimeException("Invalid number of arguments");
        String folder = args[1];
        int chunks = Integer.valueOf(args[2]);
        CloneStats stats = new CloneStats(folder, chunks);
        stats.loadProjectClones();
        stats.save();
    }

    private CloneStats(String folder, int chunks) {
        this.folder_ = folder;
        this.chunks_ = chunks;
    }



    private void loadProjectClones() {
        clones_ = new HashMap<>();
        for (int i = 0; i < chunks_; ++i) {
            String filename = folder_ + "/project_clones." + String.valueOf(i) + ".csv";
            System.out.println("Analyzing project clones " + filename + "...");
            int total = CSVReader.file(filename, (ArrayList<String> row) -> {
                if ((Integer.parseInt(row.get(2)) >= 10) && (Double.parseDouble(row.get(3)) == 100) && (Double.parseDouble(row.get(7)) == 100)) {
                    int pid = Integer.parseInt(row.get(0));
                    if (clones_.containsKey(pid)) {
                        clones_.put(pid, clones_.get(pid) + 1);
                    } else {
                        clones_.put(pid, 1);
                    }
                }
            });
            System.out.println("    total records:           " + total);
        }
    }

    private void save() {
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder_ + "/fully_cloned_projects.csv"), "utf-8"))) {
            for (Map.Entry<Integer, Integer> i : clones_.entrySet()) {
                writer.write(String.valueOf(i.getKey()));
                writer.write(",");
                writer.write(String.valueOf(i.getValue()));
                writer.write("\n");
            };
            System.out.println("    cloned projects:        " + clones_.size());
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    private String folder_;
    private int chunks_;
    private Map<Integer, Integer> clones_;
}
