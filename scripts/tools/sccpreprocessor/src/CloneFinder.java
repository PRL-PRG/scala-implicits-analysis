import java.io.*;
import java.util.*;

/**
 * Created by peta on 7.1.17.
 */
public class CloneFinder {

    public static void help() {
        System.out.println("cf FOLDER THREADS");
        System.out.println("    Calculates clone finding.");
        System.out.println("    FOLDER - folder where to look for the CSV files");
        System.out.println("    THREADS - num of threads");
        System.out.println("");
    }

    public static void find(String [] args) throws IOException, InterruptedException {
        if (args.length < 3)
            throw new RuntimeException("Invalid number of arguments");
        String folder = args[1];
        int threads = Integer.valueOf(args[2]);
        CloneFinder cf = new CloneFinder(folder, threads);
        //cf.translateHash(4);
        cf.findClones();
    }

    private CloneFinder(String folder, int numThreads) {
        folder_ = folder;
        numThreads_ = numThreads;
    }



    class File {
        int id_;
        int fileHash_;
        int tokenHash_;

        File(int id, int fileHash, int tokenHash) {
            id_ = id;
            fileHash_ = fileHash;
            tokenHash_ = tokenHash;
        }
    }

    class Project {
        int id_;
        int numFiles_;
        Map<Integer, Set<File>> files_;

        Project(int id) {
            id_ = id;
            numFiles_ = 0;
            files_ = new HashMap<Integer, Set<File>>();
        }

        void addFile(File file) {
            Set<File> f = files_.get(file.tokenHash_);
            if (f == null) {
                f = new HashSet<>();
                files_.put(file.tokenHash_, f);
            }
            f.add(file);
            ++numFiles_;
        }
    }

    class FileClone {
        int fileId_;
        int projectId_;

        FileClone(int fileId, int projectId) {
            fileId_ = fileId;
            projectId_ = projectId;
        }

    }


    // file id, project id, tokens, fileHash, tokenHash
    void translateHash(int id) {
        hashToInt_ = new HashMap<String, Integer>();
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder_ + "/" + Config.CLONE_FINDER + ".translated.csv"), "utf-8"))) {
            String filename = folder_ + "/" + Config.CLONE_FINDER + ".tmp.csv";
            int total = CSVReader.file(filename, (ArrayList<String> row) -> {
                String x = row.get(id);
                int i = hashToInt_.getOrDefault(x, -1);
                if (i == -1) {
                    i = hashToInt_.size();
                    hashToInt_.put(x, i);
                }
                row.set(id, String.valueOf(i));
                Helpers.writeRow(row, writer);
            });

            System.out.println("    files:         " + total);
            System.out.println("    unique hashes: " + hashToInt_.size());


        } catch (IOException e) {
            e.printStackTrace();
        }
        hashToInt_ = null;
    }

    void findClones() throws IOException, InterruptedException {
        projects_ = new HashMap<Integer, Project>();
        // first load all the information into memory
        String filename = folder_ + "/" + Config.CLONE_FINDER + ".translated.csv";
        int total = CSVReader.file(filename, (ArrayList<String> row) -> {
            int fileId = Integer.parseInt(row.get(0));
            int projectId = Integer.parseInt(row.get(1));
            int tokens = Integer.parseInt(row.get(2));
            int fileHash = Integer.parseInt(row.get(3));
            int tokenHash = Integer.parseInt(row.get(4));
            Project p = projects_.get(projectId);
            if (p == null) {
                p = new Project(projectId);
                projects_.put(projectId, p);
            }
            if (tokens > TOKEN_THRESHOLD) {
                File f = new File(fileId, fileHash, tokenHash);
                p.addFile(f);
            } else {
                ++p.numFiles_; // if the file is too small, only increase the numFiles.
            }
        });
        System.out.println("    files:      " + total);
        System.out.println("    projects:   " + projects_.size());
        // now let's do the same stuff the python file does




        Worker[] workers = new Worker[numThreads_];
        for (int i = 0; i < numThreads_; ++i)
            workers[i] = new Worker(i, numThreads_);
        // distribute workload
        int j = 0;
        for (Project p : projects_.values()) {
            workers[j++].addProject(p);
            j = j % numThreads_;
        }

        for (int i = 0; i < numThreads_; ++i)
            workers[i].start();
        for (int i = 0; i < numThreads_; ++i)
            workers[i].join();
    }

    class Worker extends Thread {


        Worker(int stride, int strideCount) {
            stride_ = stride;
            strideCount_ = strideCount;
            work_ = new ArrayList<>();
        }

        void addProject(Project p) {
            work_.add(p);
        }

        public void run() {
            try {
                w_ = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder_ + "/" + Config.PROJECT_CLONES + "." + stride_ + ".csv"), "utf-8"));
            long start = System.currentTimeMillis();
            int done = 0;
            for (Project p : work_) {
                if (++done % 100 == 0) {
                    System.out.println("worker " + stride_ + ": " + ((System.currentTimeMillis() - start) / 1000 + " -- " + done + String.format("  %.2f", (done * 100.0) / work_.size())));
                }
                find_clones_for_project(p);
            }
            w_.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }



        void findAllTokenHashClones(Project p, Map<Integer, Set<File>> token_hashes, Map<Integer, Set<FileClone>> files_clones) {
            // for all projects with pid > p.id_
            // for all files in them that have tokenHash that is in f
            for (Project p2: projects_.values()) {
                if (p2.id_ >= p.id_) {
                    // for each file in the other project
                    for (Map.Entry<Integer, Set<File>> i: p2.files_.entrySet()) {
                        Set<File> pfiles = token_hashes.get(i.getKey());
                        if (pfiles != null) {
                            // pfiles are all files with identical thash in project p
                            // i.getValue() are all files with identical thash in project p2
                            for (File pf : pfiles) {
                                Set<FileClone> clones = files_clones.get(pf.id_);
                                if (clones == null)
                                    System.out.println("oops");
                                for (File of: i.getValue()) {
                                    if (pf != of)
                                        clones.add(new FileClone(of.id_, p2.id_));
                                }
                            }
                        }
                    }
                }
            }
        }

        // projectFileCounts tells me # of files in the project
        void find_clones_for_project(Project p) throws IOException {
            Map<Integer, Set<FileClone>> files_clones = new HashMap(); // fileId -> set ( [fileId, projectId])
            Map<Integer, File> files_hashes = new HashMap<>(); // fileId -> its hashes
            Map<Integer, Set<File>> token_hashes = p.files_; // tokenHash -> all files within the project
            // fill in the structures
            for (Set<File> files : token_hashes.values()) {
                for (File f : files) {
                    files_clones.put(f.id_, new HashSet<>());
                    files_hashes.put(f.id_, f);
                }
            }
            int total_files = p.numFiles_;

            findAllTokenHashClones(p, token_hashes, files_clones);

            Map<Integer, Integer> percentage_clone_projects_counter = new HashMap<>(); // pid -> num of files from current project
            Map<Integer, Integer> percentage_host_projects_counter = new HashMap<>(); // pid -> num of files from other project in this project

            Map<Integer, Set<Integer>> project_file_set = new HashMap<>(); // project id -> fileId
            Set<FileClone> clone_set = new HashSet<>();

            for(Map.Entry<Integer, Set<FileClone>> i : files_clones.entrySet()) {
                for (FileClone c : i.getValue()) {
                    clone_set.add(c);
                    Set<Integer> s = project_file_set.get(c.projectId_);
                    if (s == null) {
                        s = new HashSet<>();
                        project_file_set.put(c.projectId_, s);
                    }
                    s.add(i.getKey());
                }
            }

            // how many of this project's files are present in each of the other projects
            for (Map.Entry<Integer, Set<Integer>> i : project_file_set.entrySet()) {
                percentage_clone_projects_counter.put(i.getKey(), i.getValue().size());
            }

            // how many of the other projects files are present in this project
            for (FileClone fc : clone_set) {
                int x = percentage_host_projects_counter.getOrDefault(fc.projectId_, 0);
                percentage_host_projects_counter.put(fc.projectId_, x + 1);
            }

            if (! percentage_host_projects_counter.isEmpty()) {
                // The key k (projects) should be the same between
                // percentage_clone_projects_counter and precentage_host_projects_counter
                for (Map.Entry<Integer, Integer> i : percentage_host_projects_counter.entrySet()) {
                    int k = i.getKey();
                    int v = i.getValue();
                    double percent_cloning = (percentage_clone_projects_counter.get(k) * 100.0) / total_files;
                    double percent_host = v * 100.0 / projects_.get(k).numFiles_;

                    // don't store insignificant clones
                    if (percent_cloning < 50 && percent_host < 50)
                        continue;

                    // now output to the database
                    // id
                    // cloneId 0
                    // cloneClonedFiles 1
                    // cloneTotalFiles 2
                    // cloneCloningPercent 3
                    // hostId 4
                    // hostAffectedFiles 5
                    // hostTotalFiles 6
                    // hostAffectedPercent 7
                    w_.write(String.valueOf(p.id_));
                    w_.write(",");
                    w_.write(String.valueOf(percentage_clone_projects_counter.get(k)));
                    w_.write(",");
                    w_.write(String.valueOf(total_files));
                    w_.write(",");
                    w_.write(String.format("%.2f", percent_cloning));
                    w_.write(",");
                    w_.write(String.valueOf(k));
                    w_.write(",");
                    w_.write(String.valueOf(v));
                    w_.write(",");
                    w_.write(String.valueOf(projects_.get(k).numFiles_));
                    w_.write(",");
                    w_.write(String.format("%.2f", percent_host));
                    w_.write("\n");
                }
            }
        }



        int stride_;
        int strideCount_;
        Writer w_;
        ArrayList<Project> work_;



    }




    /*
    TOKEN_THRESHOLD being 1 with > means that >=2 are taken (!!)
     */
    int TOKEN_THRESHOLD = 2;

    int numThreads_;

    String folder_;



    Map<String, Integer> hashToInt_;

    Map<Integer, Project> projects_;
}

