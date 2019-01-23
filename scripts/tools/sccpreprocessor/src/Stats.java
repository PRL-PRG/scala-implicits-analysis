import java.io.*;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

/**
 * Created by peta on 20.12.16.
 */
public class Stats {

    public static void help() {
        System.out.println("stats FILE LANGUAGE");
        System.out.println("    Prints statistics for projects available in given ghtorrent snapshot file.");
        System.out.println("    FILE - file with snapshot of ghtorrent's projects table in csv format");
        System.out.println("    LANGUAGE - only projects of this language will be considered");
        System.out.println("");
    }

    public static void print(String[] args) {
        if (args.length < 3)
            throw new RuntimeException("Invalid number of arguments");
        String file = args[1];
        String lang = args[2];
        Stats s = new Stats(file, lang);
        s.statsProjects();
    }

    private Stats(String folder, String language) {
        file_ = folder;
        language_ = language;
    }

    private void statsProjects() {
        HashSet<String> uniqueUrls = new HashSet<>();
        int total = CSVReader.file(file_, (ArrayList<String> row) -> {
            //System.out.println(row.get(5));
            if (row.get(5).equals(language_)) {
                if (! row.get(9).equals("0")) { // not deleted
                    ++projects_;
                    if (! row.get(7).equals("\\N")) {
                        uniqueUrls.add(row.get(1));
                    } else {
                        ++forks_;
                    }
                }
            }
        });
        System.out.println("Projects (total)         " + projects_);
        System.out.println("Projects (non-fork)      " + (projects_ - forks_));
        System.out.println("Unique URLs              " + uniqueUrls.size());
    }




/*
    std::string const & projectLanguage(std::vector<std::string> const & row) {
        return row[5];
    }

    bool isDeleted(std::vector<std::string> const & row) {
        return row[9] == "0";

    }

    bool isForked(std::vector<std::string> const & row) {
        return row[7] != "\\N";

    }
   */


    private String file_;
    private String language_;
    private long projects_ = 0;
    private long forks_ = 0;

}
