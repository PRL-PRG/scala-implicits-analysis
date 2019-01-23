import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

/**
 * Created by peta on 27.7.17.
 */
public class THashDist {

    public static void analyze(String[] args) {
        if (args.length != 4)
            throw new RuntimeException("Invalid number of arguments");
        String folder = args[1];
        int fhHint = Integer.parseInt(args[2]);
        int thHint = Integer.parseInt(args[3]);
        THashDist dist = new THashDist(folder, fhHint, thHint);
        dist.loadFhGroups();
        dist.loadThGroups();
        dist.analyzeGroups();
    }

    private THashDist(String folder, int fhHint, int thHint) {
        this.folder_ = folder;
        this.fhHint_ = fhHint;
        this.thHint_ = thHint;
    }




    private void loadFhGroups() {
        fhGroups_ = new int[this.fhHint_];
        thGroups_ = new Object[this.thHint_];
        String filename = folder_ + "/files.csv.h2i";
        System.out.println("Analyzing file hash groups " + filename + "...");
        int total = CSVReader.file(filename, (ArrayList<String> row) -> {
            int fh = Integer.parseInt(row.get(3));
            ++fhGroups_[fh];
        });
        System.out.println("    files analyzed:           " + total);
    }

    private void loadThGroups() {
        String filename = folder_ + "/stats.csv.h2i";
        System.out.println("Analyzing token hash groups " + filename + "...");
        int total = CSVReader.file(filename, (ArrayList<String> row) -> {
            int fh = Integer.parseInt(row.get(0));
            int th = Integer.parseInt(row.get(7));
            if (thGroups_[th] == null)
                thGroups_[th] = new HashSet<Integer>();
            ((HashSet<Integer>) thGroups_[th]).add(fh);
        });
        System.out.println("    file hashes analyzed:           " + total);
    }

    private void analyzeGroups() {
    }


/*
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
*/
    private String folder_;
    private int fhHint_;
    private int thHint_;
    private int[] fhGroups_;
    private Object[] thGroups_;

}
