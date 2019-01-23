import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;

/**
 * Created by peta on 3.1.17.
 */
public class NodeModules {

    public static void help() {
        System.out.println("nm FOLDER");
        System.out.println("    Calculates nm module index for each file.");
        System.out.println("    FOLDER - folder where to look for the CSV files");
        System.out.println("");
    }

    public static void calculate(String [] args) {
        if (args.length < 2)
            throw new RuntimeException("Invalid number of arguments");
        String folder = args[1];
        NodeModules nm = new NodeModules(folder);
        //g.createSourcererCloneGroups();
        nm.calculateNmIndex();
    }

    private NodeModules(String folder) {
        folder_ = folder;
    }





    private void calculateNmIndex() {
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder_ + "/" + Config.FILES_NM + ".csv"), "utf-8"))) {
            maxDepth_ = 0;
            numNM_ = 0;
            errors_ = 0;
            String filename = folder_ + "/" + Config.FILES + ".csv";
            int total = CSVReader.file(filename, (ArrayList<String> row) -> {
                int id = Integer.valueOf(row.get(0));
                String url = row.get(2);
                String[] s = url.split("/");
                int depth = 0;
                int test = 0;
                int locale = s[s.length-1].indexOf("locale-") != -1 ? 1 : 0;
                String name = "";
                String blame = "";
                String fname = "";
                String fext = "";
                String last = s[s.length - 1];
                String inModuleName = "";
                int lastNMIndex = -1;
                if (last.endsWith(".min.js")) {
                    fext = "min.js";
                    fname = s[s.length-1].substring(0, last.length() - 7);
                } else if (last.endsWith(".js")) {
                    fext = "js";
                    fname = s[s.length-1].substring(0, last.length() - 3);
                } else {
                    fname = last;
                    ++errors_;
                }
                for (int i = 0; i < s.length; ++i) {
                    if (s[i].equals( "node_modules")) {
                        ++depth;
                        if (i+2 < s.length) {
                            name = s[i + 1];
                            lastNMIndex = i + 2;
                        }
                        if (depth == 1)
                            blame = name;
                    } else if (s[i].equals("test") || s[i].equals("tests")) {
                        test = 1;
                    } else if (s[i].equals("locale") || s[i].equals("locales")) {
                        locale = 1;
                    }
                }
                if (lastNMIndex > -1) {
                    inModuleName = String.join("/",Arrays.copyOfRange(s, lastNMIndex, s.length));
                }
                if (depth > 0) {
                    ++numNM_;
                    if (depth > maxDepth_)
                        maxDepth_ = depth;
                }
                writer.write(row.get(0)); // file id
                writer.write(",");
                writer.write(String.valueOf(s.length)); // depth of hierarchy
                writer.write(",");
                writer.write(String.valueOf(depth)); // depth of npm hierarchy
                writer.write(",");
                writer.write(String.valueOf(test)); // 1 if the file is possibly a test
                writer.write(",");
                writer.write(String.valueOf(locale)); // 1 if the file is possibly a locale
                writer.write(",\"");
                writer.write(name); // name of the module
                writer.write("\",\"");
                writer.write(blame); // first npm module in the hierarchy
                writer.write("\",\"");
                writer.write(fname); // name of the file
                writer.write("\",\"");
                writer.write(fext); // extension
                writer.write("\",\"");
                writer.write(inModuleName); // inModuleName
                writer.write("\"\n");
            });
            System.out.println("    analyzed files:      " + total);
            System.out.println("    node module files:   " + numNM_);
            System.out.println("    max depth:           " + maxDepth_);
            System.out.println("    invalid names:       " + errors_);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private int maxDepth_;
    private int numNM_;
    private int errors_;
    private String folder_;

}
