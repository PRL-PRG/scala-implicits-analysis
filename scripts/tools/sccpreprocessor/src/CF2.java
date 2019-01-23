import java.io.*;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Created by peta on 7.1.17.
 */
public class CF2 {

    int TOKEN_THRESHOLD = 2;

    public static void find(String [] args) throws IOException, InterruptedException {
        if (args.length < 3)
            throw new RuntimeException("Invalid number of arguments");
        String folder = args[1];
        int threads = Integer.valueOf(args[2]);
        CF2 cf = new CF2(folder, threads);
        //cf.translateHash(4);
        cf.loadData();
        cf.runWorkers();
    }

    private CF2(String folder, int numThreads) {
        folder_ = folder;
        numThreads_ = numThreads;
    }



    class Project {
        int id;
        Map<Integer, Integer> files;
        int numFiles;

        Project(int id) {
            this.id = id;
            this.files = new HashMap<>();
            this.numFiles = 0;
        }

        void addFile(int tokenHash, int tokens) {
            ++numFiles;
            if (tokens >= TOKEN_THRESHOLD) {
                int i = files.getOrDefault(tokenHash, 0);
                files.put(tokenHash, i + 1);
            }
        }
    }

    void loadData()  {
        projectsMap_ = new HashMap<>();
        projectsTmp_ = new ArrayList<>();
        // first load all the information into memory
        String filename = folder_ + "/" + Config.CLONE_FINDER + ".translated.csv";
        int total = CSVReader.file(filename, (ArrayList<String> row) -> {
            int fileId = Integer.parseInt(row.get(0));
            int projectId = Integer.parseInt(row.get(1));
            int tokens = Integer.parseInt(row.get(2));
            int fileHash = Integer.parseInt(row.get(3));
            int tokenHash = Integer.parseInt(row.get(4));
            Project p = projectsMap_.get(projectId);
            if (p == null) {
                p = new Project(projectId);
                projectsMap_.put(projectId, p);
                projectsTmp_.add(p.id);
            }
            p.addFile(tokenHash, tokens);
        });
        System.out.println("    files:      " + total);
        System.out.println("    projects:   " + projectsMap_.size());
        projects_ = new int[projectsTmp_.size()];
        for (int i = 0, e = projectsTmp_.size(); i < e; ++i)
            projects_[i] = projectsTmp_.get(i);
        projectsTmp_ = null;
        System.out.println("    data loaded");
    }

    void runWorkers() throws InterruptedException {
        done_ = new AtomicInteger(0);
        Worker[] workers = new Worker[numThreads_];
        for (int i = 0; i < numThreads_; ++i)
            workers[i] = new Worker(i, numThreads_);
        for (int i = 0; i < numThreads_; ++i)
            workers[i].start();
        Printer p = new Printer();
        p.start();
        p.join();
    }

    class Printer extends Thread {
        public void run() {
            long start = System.currentTimeMillis();
            while (done_.get() < projects_.length) {
                try {
                    TimeUnit.SECONDS.sleep(10);
                } catch (InterruptedException e) {
                }
                System.out.println("Done " + done_.get() + " (" + String.format(" %.2f", done_.get() * 100.0 / projects_.length) + "%) in " + ((System.currentTimeMillis() - start) / 1000) + "[s]");
            }
        }
    }

    class Worker extends Thread {

        Worker(int stride, int strideCount) {
            stride_ = stride;
            strideCount_ = strideCount;
            work_ = new ArrayList<>();
        }

        public void run() {
            try {
                w_ = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder_ + "/" + Config.PROJECT_CLONES + "." + stride_ + ".csv"), "utf-8"));
                for (int i = stride_; i < projects_.length; i += strideCount_) {
                    findClonesForProject(i);
                    done_.incrementAndGet();
                }
                w_.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        class FileClone {
            int projectId;
            int numFiles;

            FileClone(int projectId, int numFiles) {
                this.projectId = projectId;
                this.numFiles = numFiles;
            }
        }

        void findAllTokenHashClones(Project p, int index, Map<Integer, ArrayList<FileClone>> files_clones) {
            // first look for clones in the same project, if we have some of the files multiple times
            for (Map.Entry<Integer, Integer> i : p.files.entrySet())
                if (i.getValue() > 1)
                    files_clones.get(i.getKey()).add(new FileClone(p.id, i.getValue() - 1));
            // then look for clones in other projects
            ++index;
            for (index = index + 1; index < projects_.length; ++index) {
                Project p2 = projectsMap_.get(projects_[index]);
                // for all token hashes that I have
                for (int tokenHash : p.files.keySet()) {
                    int otherHas = p2.files.getOrDefault(tokenHash, 0);
                    if (otherHas > 0)
                        files_clones.get(tokenHash).add(new FileClone(p2.id, otherHas));
                }
            }
        }

        void findClonesForProject(int index) throws IOException {
            Project p = projectsMap_.get(projects_[index]);

            // for each file in the project this contains for each project that contains a copy the id of the project and number of the copies in that project
            Map<Integer, ArrayList<FileClone>> files_clones = new HashMap<>();

            // initialize file clones for files in the current project
            for (int tokenHash : p.files.keySet())
                files_clones.put(tokenHash, new ArrayList<>());

            findAllTokenHashClones(p, index, files_clones);

            // these will be percentages

            Map<Integer, Integer> percentage_clone_projects_counter = new HashMap<>();
            Map<Integer, Integer> percentage_host_projects_counter = new HashMap<>();


            Set<FileClone> clone_set = new HashSet<>();

            for (Map.Entry<Integer, ArrayList<FileClone>> i : files_clones.entrySet()) {
                ArrayList<FileClone> clones = i.getValue();
                int tokenHash = i.getKey();
                for (FileClone c: clones) {
                    // how many of this project's files are present in each of the other projects, i.e. look at what is in the FileClone
                    int x = percentage_clone_projects_counter.getOrDefault(c.projectId, 0);
                    //percentage_clone_projects_counter.put(c.projectId, x + c.numFiles);
                    percentage_clone_projects_counter.put(c.projectId, x + p.files.get(tokenHash));

                    // how many of the other's project files are present in the current project, i.e. look at the # of that tokenHash files in the current project
                    x = percentage_host_projects_counter.getOrDefault(c.projectId, 0);
                    //percentage_host_projects_counter.put(c.projectId, p.files.get(tokenHash) + x);
                    percentage_host_projects_counter.put(c.projectId, x + c.numFiles);
                }
                // TODO perhaps these two are just the other way round!!!!
            }

            if (! percentage_host_projects_counter.isEmpty()) {
                for (Map.Entry<Integer, Integer> i : percentage_host_projects_counter.entrySet()) {
                    int k = i.getKey();
                    int v = i.getValue();
                    double percent_cloning = (percentage_clone_projects_counter.get(k) * 100.0) / p.numFiles;
                    double percent_host = v * 100.0 / projectsMap_.get(k).numFiles;

                    // don't store insignificant clones
                    if (percent_cloning < 50 && percent_host < 50)
                        continue;
                    if ( percent_host > 100 || percent_cloning > 100)
                        throw new RuntimeException("");

                    // now output to the database
                    // id
                    // cloneId
                    // cloneClonedFiles
                    // cloneTotalFiles
                    // cloneCloningPercent
                    // hostId
                    // hostAffectedFiles
                    // hostTotalFiles
                    // hostAffectedPercent
                    w_.write(String.valueOf(p.id));
                    w_.write(",");
                    w_.write(String.valueOf(percentage_clone_projects_counter.get(k)));
                    w_.write(",");
                    w_.write(String.valueOf(p.numFiles));
                    w_.write(",");
                    w_.write(String.format("%.2f", percent_cloning));
                    w_.write(",");
                    w_.write(String.valueOf(k));
                    w_.write(",");
                    w_.write(String.valueOf(v));
                    w_.write(",");
                    w_.write(String.valueOf(projectsMap_.get(k).numFiles));
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

    String folder_;
    int numThreads_;

    int[] projects_;
    HashMap<Integer, Project> projectsMap_;
    ArrayList<Integer> projectsTmp_;

    AtomicInteger done_;

}
