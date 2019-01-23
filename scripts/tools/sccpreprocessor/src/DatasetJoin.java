import java.io.*;
import java.util.HashSet;
import java.util.Set;

/**
 * Created by peta on 20.12.16.
 */
public class DatasetJoin {

    public static void help() {
        System.out.println("join FOLDER_1 FOLDER_2 RESULT");
        System.out.println("    Joins two datasets, keeping all projects and files from the firstone and adding those from second that are not in first.");
        System.out.println("    FOLDER_1 - folder containing the first files, projects & stats");
        System.out.println("    FOLDER_2 - folder containing the secon files, projects & stats");
        System.out.println("    RESULT - folder to which the joined dataset will be stored");
        System.out.println("");
    }

    public static void join(String[] args) {
        if (args.length < 4)
            throw new RuntimeException("Invalid number of arguments");
        String folder1 = args[1];
        String folder2 = args[2];
        String folderOut = args[3];

        DatasetJoin j = new DatasetJoin(folder1, folder2, folderOut);
        j.joinProjects();
        j.joinProjectsExtra();
        j.joinFiles();
        j.joinFilesExtra();
        j.joinStats("generic");
        j.joinStats("js");
    }


    private DatasetJoin(String folder1, String folder2, String folderOut) {
        folder1_ = folder1;
        folder2_ = folder2;
        folderOut_ = folderOut;
        firstProjects_ = new HashSet<Long>();
        keepProjects_ = new HashSet<Long>();
        keepFiles_ = new HashSet<Long>();
    }

    /** Takes all projects from first and those projects from second that are not in first.
     */
    private void joinProjects() {
        String file1 = folder1_ + "/" + Config.PROJECTS + ".csv";
        String file2 = folder2_ + "/" + Config.PROJECTS + ".csv";
        String fileOut = folderOut_ + "/" + Config.PROJECTS + ".csv";

        System.out.println("Joining project information...");
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileOut), "utf-8"))) {
            System.out.println("  outputing all projects from first: " + file1);
            int total = LineReader.file(file1, (String line) -> {
                Long pid = Long.parseLong(line.split(",",2)[0]);
                firstProjects_.add(pid);
                writer.write(line);
                writer.write("\n");
            });
            System.out.println("    " + total + " projects written");
            System.out.println("  outputing new projects from second: " + file2);
            used_ = 0;
            total = LineReader.file(file2, (String line) -> {
                Long pid = Long.parseLong(line.split(",", 2)[0]);
                // only output if it does not contain
                if (! firstProjects_.contains(pid)) {
                    keepProjects_.add(pid);
                    ++used_;
                    writer.write(line);
                    writer.write("\n");
                }
            });
            System.out.println("    " + total + " projects in second");
            System.out.println("    " + used_ + " projects kept");
        } catch (IOException e) {
            e.printStackTrace();
        }
        // cleanup
        firstProjects_ = null;
    }

    private void joinProjectsExtra() {
        String file1 = folder1_ + "/" + Config.PROJECTS_EXTRA + ".csv";
        String file2 = folder2_ + "/" + Config.PROJECTS_EXTRA + ".csv";
        String fileOut = folderOut_ + "/" + Config.PROJECTS_EXTRA + ".csv";

        System.out.println("Joining project extra information...");
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileOut), "utf-8"))) {
            System.out.println("  outputing all projects from first: " + file1);
            int total = LineReader.file(file1, (String line) -> {
                writer.write(line);
                writer.write("\n");
            });
            System.out.println("    " + total + " projects written");
            System.out.println("  outputing new projects from second: " + file2);
            used_ = 0;
            total = LineReader.file(file2, (String line) -> {
                Long pid = Long.parseLong(line.split(",", 2)[0]);
                if (keepProjects_.contains(pid)) {
                    ++used_;
                    writer.write(line);
                    writer.write("\n");
                }
            });
            System.out.println("    " + total + " projects in second");
            System.out.println("    " + used_ + " projects kept");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void joinFiles() {
        String file1 = folder1_ + "/" + Config.FILES + ".csv";
        String file2 = folder2_ + "/" + Config.FILES + ".csv";
        String fileOut = folderOut_ + "/" + Config.FILES + ".csv";

        System.out.println("Joining file information...");
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileOut), "utf-8"))) {
            System.out.println("  outputing all files from first: " + file1);
            int total = LineReader.file(file1, (String line) -> {
                writer.write(line);
                writer.write("\n");
            });
            System.out.println("    " + total + " files written");
            System.out.println("  outputing files from new projects in second: " + file2);
            used_ = 0;
            total = LineReader.file(file2, (String line) -> {
                Long pid = Long.parseLong(line.split(",", 3)[1]);
                if (keepProjects_.contains(pid)) {
                    keepFiles_.add(Long.parseLong(line.split(",", 2)[0]));
                    ++used_;
                    writer.write(line);
                    writer.write("\n");
                }
            });
            System.out.println("    " + total + " files in second");
            System.out.println("    " + used_ + " files kept");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void joinFilesExtra() {
        String file1 = folder1_ + "/" + Config.FILES_EXTRA + ".csv";
        String file2 = folder2_ + "/" + Config.FILES_EXTRA + ".csv";
        String fileOut = folderOut_ + "/" + Config.FILES_EXTRA + ".csv";

        System.out.println("Joining file extra information...");
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileOut), "utf-8"))) {
            System.out.println("  outputing all files from first: " + file1);
            int total = LineReader.file(file1, (String line) -> {
                writer.write(line);
                writer.write("\n");
            });
            System.out.println("    " + total + " files written");
            System.out.println("  outputing files from new projects in second: " + file2);
            used_ = 0;
            total = LineReader.file(file2, (String line) -> {
                Long id = Long.parseLong(line.split(",", 2)[0]);
                if (keepFiles_.contains(id)) {
                    ++used_;
                    writer.write(line);
                    writer.write("\n");
                }
            });
            System.out.println("    " + total + " files in second");
            System.out.println("    " + used_ + " files kept");
        } catch (IOException e) {
            e.printStackTrace();
        }
        keepFiles_ = null;
    }

    private void joinStats(String tokenizer) {
        String file1;
        String file2;
        String fileOut;
        if (tokenizer == "generic") {
            file1 = folder1_ + "/" + Config.GENERIC_STATS + ".csv";
            file2 = folder2_ + "/" + Config.GENERIC_STATS + ".csv";
            fileOut = folderOut_ + "/" + Config.GENERIC_STATS + ".csv";
        } else if (tokenizer == "js") {
            file1 = folder1_ + "/" + Config.GENERIC_STATS + ".csv";
            file2 = folder2_ + "/" + Config.GENERIC_STATS + ".csv";
            fileOut = folderOut_ + "/" + Config.GENERIC_STATS + ".csv";
        } else {
            throw new RuntimeException("Invalid tokenizer " + tokenizer);
        }

        firstHashes_ = new HashSet<String>();
        System.out.println("Joining file stats information for " + tokenizer + " tokenizer...");
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileOut), "utf-8"))) {
            System.out.println("  outputing all stats from first: " + file1);
            int total = LineReader.file(file1, (String line) -> {
                String hash = line.split(",", 2)[0];
                firstHashes_.add(hash);
                writer.write(line);
                writer.write("\n");
            });
            System.out.println("    " + total + " stats written");
            System.out.println("  outputing new stats in second: " + file2);
            used_ = 0;
            total = LineReader.file(file2, (String line) -> {
                String hash = line.split(",", 2)[0];
                if (!firstHashes_.contains(hash)) {
                    ++used_;
                    writer.write(line);
                    writer.write("\n");
                }
            });
            System.out.println("    " + total + " stats in second");
            System.out.println("    " + used_ + " stats kept");
        } catch (IOException e) {
            e.printStackTrace();
        }
        firstHashes_ = null;
    }

    private void cleanJsStats() {

    }

    private Set<Long> firstProjects_;
    private Set<Long> keepProjects_;
    private Set<Long> keepFiles_;
    private Set<String> firstHashes_;

    private String folder1_;
    private String folder2_;
    private String folderOut_;
    private long used_;
}
