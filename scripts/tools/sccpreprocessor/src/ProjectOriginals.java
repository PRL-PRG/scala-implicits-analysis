
/* Calculates project originals, i.e. for

 */

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

public class ProjectOriginals {
    public static void help() {
        System.out.println("originals FOLDER");
        System.out.println("    Does project originals calculation.");
        System.out.println("    FOLDER - folder where to look for the CSV files");
        System.out.println("");
    }


    public static void analyze(String [] args) {
        if (args.length < 3)
            throw new RuntimeException("Invalid number of arguments");
        String folder = args[1];
        int chunks = Integer.parseInt(args[2]);
        ProjectOriginals po = new ProjectOriginals(folder, chunks);
        po.loadProjects();
        po.loadStatsCount();
        po.loadProjectClones();
        po.saveData();
    }

    private ProjectOriginals(String folder, int chunks) {
        folder_ = folder;
        chunks_ = chunks;
    }


    /* For each project, the following is remembered:
     */
    private class Record {
        int stars;
        int commits;
        int files;
        int originalFiles;
        int clonesContained;
        //HashSet<Integer> similarProjects;

        Record(int stars, int commits) {
            this.stars = stars;
            this.commits = commits;
            files = 0;
            originalFiles = 0;
            clonesContained = 0;
            //similarProjects = new HashSet<>();
        }
    }





    /* Loads all projects.
     */
    private void loadProjects() {
        projects_ = new HashMap<>();
        String filename = folder_ + "/projects_heat.csv";
        System.out.println("Loading projects...");
        int total = CSVReader.file(filename, (ArrayList<String> row) -> {
            int id = Integer.parseInt(row.get(0));
            int stars = Integer.parseInt(row.get(1));
            if (stars == -1)
                return;
            int commits = Integer.parseInt(row.get(2));
            if (commits == -1)
                return;
            projects_.put(id, new Record(stars, commits));
        });
        System.out.println("    total projects:          " + total);
        System.out.println("    loaded projects:         " + projects_.size());
    }

    private void loadStatsCount() {
        statsCounts_ = new HashMap<>();
        String filename = folder_ + "/files.csv.h2i";
        System.out.println("Loading file stats counts...");
        int total = CSVReader.file(filename, (ArrayList<String> row) -> {
            int pid = Integer.parseInt(row.get(1));
            int fileHash = Integer.parseInt(row.get(3));
            // increase total number of files for the project
            if (projects_.containsKey(pid))
                projects_.get(pid).files += 1;
            if (statsCounts_.containsKey(fileHash))
                statsCounts_.put(fileHash, statsCounts_.get(fileHash) + 1);
            else
                statsCounts_.put(fileHash, 1);
        });
        System.out.println("    total files:             " + total);
        System.out.println("    unique files:            " + statsCounts_.size());
        // run through files again, this time counting original files
        CSVReader.file(filename, (ArrayList<String> row) -> {
            int fileHash = Integer.parseInt(row.get(3));
            if (statsCounts_.get(fileHash) == 1) {
                int pid = Integer.parseInt(row.get(1));
                if (projects_.containsKey(pid))
                    projects_.get(pid).originalFiles += 1;
                ++originalFiles_;
            }
        });
        System.out.println("    original files:          " + originalFiles_);
    }

    private void loadProjectClones() {
        for (int i = 0; i < chunks_; ++i) {
            String filename = folder_ + "/project_clones." + String.valueOf(i) + ".csv";
            System.out.println("Analyzing project clones " + filename + "...");
            int total = CSVReader.file(filename, (ArrayList<String> row) -> {
                int cloneId = Integer.parseInt(row.get(0));
                int hostId = Integer.parseInt(row.get(4));
                double similarity = Double.parseDouble(row.get(7));
                if (similarity >= cloneThreshold) {
                    if (projects_.containsKey(hostId))
                        projects_.get(hostId).clonesContained += 1;
                }
            });
            System.out.println("    total records:           " + total);
        }
    }

    void saveData() {
        System.out.println("Saving...");
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder_ + "/heatmap.csv"), "utf-8"))) {
            for (int pid : projects_.keySet()) {
                Record r = projects_.get(pid);
                writer.write(String.valueOf(pid));
                writer.write(",");
                writer.write(String.valueOf(r.stars));
                writer.write(",");
                writer.write(String.valueOf(r.commits));
                writer.write(",");
                writer.write(String.valueOf(r.files));
                writer.write(",");
                writer.write(String.valueOf(r.originalFiles));
                writer.write(",");
                writer.write(String.valueOf(r.clonesContained));
                writer.write("\n");
            }
            System.out.println("    written:   " + projects_.size());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }



    /* When we have the projects, analyze the project clone records to find the originals.
     */
    private void analyzeProjectClones(int index) {
        String filename = folder_ + "/" + Config.PROJECT_CLONES + "." + String.valueOf(index) + ".csv";
        System.out.println("Analyzing project clones: " + filename);
        int total = CSVReader.file(filename, (ArrayList<String> row) -> {
            int cloneId = Integer.parseInt(row.get(0));
            int hostId = Integer.parseInt(row.get(4));
            double similarity = Double.parseDouble(row.get(7));
        });
        System.out.println("    total records:           " + total);
    }


/*


            // cloneId 0
            // cloneClonedFiles 1
            // cloneTotalFiles 2
            // cloneCloningPercent 3
            // hostId 4
            // hostAffectedFiles 5
            // hostTotalFiles 6
            // hostAffectedPercent 7

 */


    int originalFiles_ = 0;
    private String folder_;
    private int chunks_;
    private double cloneThreshold = 0.8;
    private HashMap<Integer, Record> projects_;
    private HashMap<Integer, Integer> statsCounts_;

}
