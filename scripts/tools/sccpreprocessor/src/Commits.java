import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by peta on 7.4.17.
 */
public class Commits {
    public static void help() {
        System.out.println("commits FOLDER COMMITS_FILE");
        System.out.println("    Calculates number of commits for projects in given dataset.");
        System.out.println("    FOLDER - folder where to look for the CSV files");
        System.out.println("    COMMITS_FILE - path to file containing project, commit ids");
        System.out.println("");
    }

    public static void calculate(String [] args) {
        if (args.length < 3)
            throw new RuntimeException("Invalid number of arguments");
        String folder = args[1];
        String commitsFile = args[2];
        Commits c = new Commits(folder, commitsFile);
        c.loadProjects();
        c.calculateCommits();
        c.saveData();
    }

    private Commits(String folder, String commitsFile) {
        folder_ = folder;
        commitsFile_ = commitsFile;
    }







    private void loadProjects() {
        commits_ = new HashMap<>();
        String filename = folder_ + "/" + Config.PROJECTS + ".csv";
        System.out.println("Loading projects...");
        int total = CSVReader.file(filename, (ArrayList<String> row) -> {
            commits_.put(Integer.parseInt(row.get(0)), 0);
        });
        System.out.println("    total projects:          " + total);
    }
    private void calculateCommits() {
        System.out.println("Loading project commits...");
        analyzed_ = 0;
        int total = CSVReader.file(commitsFile_, (ArrayList<String> row) -> {
            int pid = Integer.parseInt(row.get(0));
            if (commits_.containsKey(pid)) {
                commits_.put(pid, commits_.get(pid) + 1);
                analyzed_ += 1;
            }
        });
        System.out.println("    total records:          " + total);
        System.out.println("    analyzed records:       " + analyzed_);
    }

    void saveData() {
        System.out.println("Saving commit counts...");
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder_ + "/project_commits.csv"), "utf-8"))) {
            for (int k : commits_.keySet()) {
                writer.write(String.valueOf(k));
                writer.write(",");
                writer.write(String.valueOf(commits_.get(k)));
                writer.write("\n");
            }
            System.out.println("    written:   " + commits_.size());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }




    private String folder_;
    private String commitsFile_;
    private HashMap<Integer, Integer> commits_;
    private long analyzed_;
}
