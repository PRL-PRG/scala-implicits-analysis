import java.io.*;
import java.util.ArrayList;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Created by peta on 8.1.17.
 */
public class NPMPackages {


    public static void find(String [] args) throws IOException, InterruptedException {
        if (args.length < 4)
            throw new RuntimeException("Invalid number of arguments");
        String folder = args[1];
        String outputFolder = args[2];
        int threads = Integer.valueOf(args[3]);
        NPMPackages pkgs = new NPMPackages(folder, outputFolder, threads);
        pkgs.loadData();
        pkgs.runWorkers();
        pkgs.saveData();
    }

    private NPMPackages(String folder, String outputFolder, int numThreads) {
        folder_ = folder;
        outputFolder_ = outputFolder;
        numThreads_ = numThreads;
        done_ = new AtomicInteger(0);
        npms_ = new AtomicInteger(0);
    }

    class Project {
        int id;
        String url;
        boolean isNPM;
        Project(int id, String url) {
            this.id = id;
            this.url = url;
            isNPM = false;
        }

        String getPackageJSONUrl() {
            return "https://raw.githubusercontent.com/" + url + "/master/package.json";
        }
    }

    void loadData() {
        projects_ = new ArrayList();
        String filename = folder_ + "/" + Config.PROJECTS + ".csv";
        int total = CSVReader.file(filename, (ArrayList<String> row) -> {
            int id = Integer.valueOf(row.get(0));
            String url = row.get(2);
            projects_.add(new Project(id, url));
        });
        System.out.println("    projects loaded:  " + total);
    }

    class Printer extends Thread {
        public void run() {
            long start = System.currentTimeMillis();
            while (done_.get() < projects_.size()) {
                try {
                    TimeUnit.SECONDS.sleep(10);
                } catch (InterruptedException e) {
                }
                System.out.println(((System.currentTimeMillis() - start) / 1000) + "[s]");
                System.out.println("    projects done: " + done_.get() + " (" + String.format(" %.2f", done_.get() * 100.0 / projects_.size()) + " %)");
                System.out.println("    npms:          " + npms_.get() + " (" + String.format(" %.2f", npms_.get() * 100.0 / done_.get()) + " %)");
            }
        }
    }

    void runWorkers() throws InterruptedException {
        Worker[] workers = new Worker[numThreads_];
        for (int i = 0; i < numThreads_; ++i)
            workers[i] = new Worker(i);
        for (int i = 0; i < numThreads_; ++i)
            workers[i].start();
        Printer p = new Printer();
        p.start();
        p.join();
    }

    void saveData() {
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder_ + "/" + Config.CLONE_PAIRS + "_fileHash.csv"), "utf-8"))) {
            String filename = folder_ + "/" + Config.PROJECTS_NPM + ".csv";
            for (Project p : projects_) {
                writer.write(String.valueOf(p.id));
                writer.write(",");
                writer.write(p.isNPM ? "1" : "0");
                writer.write("\n");
            }
            System.out.println("    written:   " + projects_.size());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    class Worker extends Thread {

        Worker(int stride) {
            stride_ = stride;
        }

        public void run() {
            for (int i = stride_; i < projects_.size(); i += numThreads_)
                try {
                    checkProject(projects_.get(i));
                } catch (IOException e) {
                    e.printStackTrace();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
        }

        void checkProject(Project p) throws IOException, InterruptedException {
            String url = p.getPackageJSONUrl();
            String output = outputFolder_ + "/" + p.id + ".json";
            // try to wget the package
            Process proc = Runtime.getRuntime().exec("wget " + url + " -O " + output);
            proc.waitFor();
            // to be double sure, only say it worked if the file actually has been downloaded
            File f = new File(output);
            if(f.exists() && !f.isDirectory() && f.length() > 0) {
                p.isNPM = true;
                npms_.incrementAndGet();
            }
            done_.incrementAndGet();
        }

        int stride_;
    }







    String folder_;
    String outputFolder_;
    int numThreads_;

    ArrayList<Project> projects_;

    AtomicInteger done_;
    AtomicInteger npms_;
}
