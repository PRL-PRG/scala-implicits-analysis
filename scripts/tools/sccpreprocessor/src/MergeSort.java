import java.io.*;

/**
 * Created by peta on 19.12.16.
 */
public class MergeSort {

    public static void help() {
        System.out.println("mergesort FILE_1 FILE_2 RESULT");
        System.out.println("    Merges two sorted tokenized_files into one.");
        System.out.println("    FILE_1 - first sorted tokenized file");
        System.out.println("    FILE_2 - second sorted tokenized file");
        System.out.println("    RESULT - result of the merge");
        System.out.println("");
    }

    public static void merge(String[] args) {
        if (args.length < 4)
            throw new RuntimeException("Invalid number of arguments");
        String filename1 = args[1];
        String filename2 = args[2];
        String output = args[3];
        MergeSort m = new MergeSort(filename1, filename2, output);
        m.process();
    }

    private MergeSort(String filename1, String filename2, String output) {
        filename1_ = filename1;
        filename2_ = filename2;
        outputFilename_ = output;
    }

    private void process() {
        System.out.println("Mergesort of " + filename1_ + " and " + filename2_);
        try (BufferedReader br1 = new BufferedReader(new FileReader(filename1_))) {
            try (BufferedReader br2 = new BufferedReader(new FileReader(filename2_))) {
                try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outputFilename_), "utf-8"))) {
                    String first = br1.readLine();
                    String second = br2.readLine();
                    // FIXME this assumes they are not empty and I am tired
                    long id1 = Long.parseLong(first.split(",", 3)[1]);
                    long id2 = Long.parseLong(second.split(",", 3)[1]);
                    long firstRows = 0;
                    long secondRows = 0;
                    while (true) {
                        while (id1 < id2) {
                            // output first
                            ++firstRows;
                            writer.write(first);
                            writer.write("\n");
                            first = br1.readLine();
                            if (first == null)
                                id1 = Long.MAX_VALUE;
                            else
                                id1 = Long.parseLong(first.split(",", 3)[1]);
                        }
                        while (id2 < id1) {
                            // output second
                            ++secondRows;
                            writer.write(second);
                            writer.write("\n");
                            second = br2.readLine();
                            if (second == null)
                                id2 = Long.MAX_VALUE;
                            else
                                id2 = Long.parseLong(second.split(",", 3)[1]);
                        }
                        if (first == null && second == null)
                            break;
                    }
                    System.out.println("  rows in first:  " + firstRows);
                    System.out.println("  rows in second: " + secondRows);
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }




    private String filename1_;
    private String filename2_;
    private String outputFilename_;
}
