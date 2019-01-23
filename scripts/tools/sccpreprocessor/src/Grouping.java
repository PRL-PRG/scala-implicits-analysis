import java.io.*;
import java.util.*;

/**Performs clone group detection.

 To minimize memory consumption the algorithm starts by creating a list of all sourcererCC clone groups.

 We start with searching for all soucerer clone groups.

 Once done we look at stats:


 */
public class Grouping {

    public static void help() {
        System.out.println("group FOLDER");
        System.out.println("    Creates clone groups from the tokenizer and sourcererCC outputs in given folder.");
        System.out.println("    FOLDER - folder where to look for the CSV files");
        System.out.println("");
    }


    public static void group(String [] args) {
        if (args.length < 2)
            throw new RuntimeException("Invalid number of arguments");
        String folder = args[1];
        Grouping g = new Grouping(folder);
        g.createSourcererCloneGroups();
        g.tokenHashGroups_ = null;
        g.groupMembers_ = null;
        g.groups_ = null;
        g.hashGroups_ = null;
        g.createHashCloneGroups();
    }

    private Grouping(String folder) {
        folder_ = folder;
    }

    private class HashGroup {
        int id_;
        int oldestId_ = -1;
        int oldestTime_ = Integer.MAX_VALUE;
        int size_ = 0;
        int fileHashes_ = 1;

        HashGroup(int id) {
            id_ = id;
        }

        void addMember(int id, int createdAt) {
            if (++size_ == 1 || oldestTime_ > createdAt) {
                oldestId_ = id;
                oldestTime_ = createdAt;
            }
        }

        void mergeWith(HashGroup other) {
            if (oldestTime_ > other.oldestTime_) {
                oldestTime_ = other.oldestTime_;
                oldestId_ = other.oldestId_;
            }
            size_ += other.size_;
            ++fileHashes_;
        }
    }

    private class CloneGroup {
        int id_;
        Set<Integer> members_;
        int oldestId_ = -1;
        int oldestTime_ = Integer.MAX_VALUE;
        int size_;

        CloneGroup() {
            id_ = -1;
            size_ = 0;
            members_ = new HashSet<Integer>();
        }

        void addMember(int id) {
            members_.add(id);
            ++size_;
            //groups_.put(id, this);
        }

        void updateOldestId(int id, int createdAt) {
            if (oldestId_ == -1 || oldestTime_ > createdAt) {
                oldestId_ = id;
                oldestTime_ = createdAt;
            }
        }
    }

    /** Tries to unify two clone groups. We can only do this if the second group is a subset of the first one.
     */
    private boolean tryUnify(CloneGroup a, CloneGroup b) {
        if (b == null)
            return false;
        if (a == b)
            return false;
        if (b.id_ == -2)
            return false;
        if (b.size_ > a.size_)
            return false;
        if (a.members_.containsAll(b.members_)) {
            b.id_ = -2;
            return true;
        } else {
            return false;
        }
    }

    private void createSourcererCloneGroups() {
        groups_ = new HashMap<Integer, CloneGroup>();
        String filename = folder_ + "/" + Config.SOURCERER_CLONE_PAIRS + ".csv";
        int total = CSVReader.file(filename, (ArrayList<String> row) -> {
            int first = Integer.parseInt(row.get(1));
            int second = Integer.parseInt(row.get(3));
            CloneGroup g = groups_.getOrDefault(first, null);
            if (g == null) {
                g = new CloneGroup();
                ++numGroups_;
                g.addMember(first);
                groups_.put(first, g);
            }
            g.addMember(second);
        });
        System.out.println("    total pairs:         " + total);
        System.out.println("    clone groups:        " + numGroups_);
        // now let's try to unify any groups we can
        for (Map.Entry<Integer, CloneGroup> e: groups_.entrySet()) {
            CloneGroup g = e.getValue();
            if (g.id_ == -1) {
                for (int i : g.members_) {
                    if (tryUnify(g, groups_.get(i)))
                        --numGroups_;
                }
            }
        }
        Iterator<Map.Entry<Integer,CloneGroup>> iter = groups_.entrySet().iterator();
        while (iter.hasNext()) {
            Map.Entry<Integer, CloneGroup> entry = iter.next();
            if (entry.getValue().id_ == -2) {
                iter.remove();
            }
        }
        System.out.println("    valid clone groups:  " + numGroups_);
        // translate clone groups to membership information
        groupMembers_ = new HashMap<Integer, Set<CloneGroup>>();
        for (Map.Entry<Integer, CloneGroup> e: groups_.entrySet()) {
            CloneGroup g = e.getValue();
            if (g.members_ != null) {
                for (int i : g.members_) {
                    Set<CloneGroup> m = groupMembers_.get(i);
                    if (m == null) {
                        m = new HashSet<CloneGroup>();
                        groupMembers_.put(i, m);
                    }
                    m.add(g);
                }
                g.members_ = null; // we no longer need it, let's free memory
            }
        }
        System.out.println("    group membership:    " + groupMembers_.size());
        // now for each group, get the oldest file id
        filename = folder_ + "/" + Config.FILES_EXTRA + ".csv";
        total = CSVReader.file(filename, (ArrayList<String> row) -> {
            int id = Integer.parseInt(row.get(0));
            Set<CloneGroup> g = groupMembers_.get(id);
            if (g != null) {
                int createdAt = Integer.parseInt(row.get(1));
                for (CloneGroup cg : g)
                    cg.updateOldestId(id, createdAt);
            }
        });
        System.out.println("    files checked:       " + total);
        // remove groups with no oldest files
        iter = groups_.entrySet().iterator();
        while (iter.hasNext()) {
            Map.Entry<Integer, CloneGroup> entry = iter.next();
            if (entry.getValue().oldestId_ == -1) {
                iter.remove();
                --numGroups_;
            }
        }
        System.out.println("    valid clone groups:  " + numGroups_);
        // now let's print the group pairings and name the groups
        total = 0;
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder_ + "/" + Config.CLONE_PAIRS + "_scc.csv"), "utf-8"))) {
            int gid = 0;
            for (Map.Entry<Integer, Set<CloneGroup>> e: groupMembers_.entrySet()) {
                int id = e.getKey();
                for (CloneGroup g : e.getValue()) {
                    if (g.id_ == -1)
                        g.id_ = gid++;
                    writer.write(String.valueOf(id));
                    writer.write(",");
                    writer.write(String.valueOf(g.id_));
                    writer.write("\n");
                    ++total;
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        System.out.println("    clone pairs:         " + total);
        // and now write the groups information
        numGroups_ = 0;
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder_ + "/" + Config.CLONE_GROUPS + "_scc.csv"), "utf-8"))) {
            for (Map.Entry<Integer, CloneGroup> e : groups_.entrySet()) {
                CloneGroup g = e.getValue();
                if (g.id_  >= 0 ) {
                    writer.write(String.valueOf(g.id_));
                    writer.write(",");
                    writer.write(String.valueOf(g.size_));
                    writer.write(",");
                    writer.write(String.valueOf(g.oldestId_));
                    writer.write("\n");
                    g.id_ = -1;
                    ++numGroups_;
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        System.out.println("    groups written:      " + numGroups_);
    }

    /** To do this effectively, we need for each file its id, fileHash and createdAt in a single csv file.

     Once we have this, we can construct the fileHash groups easily, we can then unify these to form the tokenHash groups.
      */
    private void createHashCloneGroups() {
        hashGroups_ = new HashMap<String, HashGroup>();
        // obtain hash groups by reading the stats
        String filename = folder_ + "/" + Config.GENERIC_STATS + ".csv";
        int total = CSVReader.file(filename, (ArrayList<String> row) -> {
            String hash = row.get(0);
            HashGroup g = new HashGroup(hashGroups_.size());
            hashGroups_.put(hash, g);
        });
        System.out.println("    analyzed records:    " + total);
        System.out.println("    file hash groups:    " + hashGroups_.size());
        // now calculate membership
        filename = folder_ + "/" + Config.FILES_ID_HASH_CREATED_AT + ".csv";
        total = CSVReader.file(filename, (ArrayList<String> row) -> {
            int id = Integer.parseInt(row.get(0));
            String hash = row.get(1);
            HashGroup g = hashGroups_.get(hash);
            if (g != null) {
                int createdAt = Integer.parseInt(row.get(2));
                g.addMember(id, createdAt);
            }
        });
        System.out.println("    analyzed files:      " + total);
        // see how many valid groups we have but do not remove them yet
        int validGroups = 0;
        Iterator<Map.Entry<String,HashGroup>> iter = hashGroups_.entrySet().iterator();
        while (iter.hasNext()) {
            Map.Entry<String, HashGroup> entry = iter.next();
            HashGroup g = entry.getValue();
            if (g.size_ > 1) {
                ++validGroups;
            } else if (g.size_ == 0) {
                iter.remove(); // remove if no valid file inside the group
            }
        }
        System.out.println("    valid hash groups:   " + validGroups);
        // we have the groups, let's print them
        numGroups_ = 0;
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder_ + "/" + Config.CLONE_GROUPS + "_fileHash.csv"), "utf-8"))) {
            for (Map.Entry<String, HashGroup> e : hashGroups_.entrySet()) {
                HashGroup g = e.getValue();
                if (g.size_ > 1) {
                    writer.write(String.valueOf(g.id_));
                    writer.write(",");
                    writer.write(String.valueOf(g.size_));
                    writer.write(",");
                    writer.write(String.valueOf(g.oldestId_));
                    writer.write("\n");
                    ++numGroups_;
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        System.out.println("    groups written:      " + numGroups_);
        // now print the membership
        numFiles_ = 0;
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder_ + "/" + Config.CLONE_PAIRS + "_fileHash.csv"), "utf-8"))) {
            filename = folder_ + "/" + Config.FILES_ID_HASH_CREATED_AT + ".csv";
            total = CSVReader.file(filename, (ArrayList<String> row) -> {
                int id = Integer.parseInt(row.get(0));
                String hash = row.get(1);
                HashGroup g = hashGroups_.get(hash);
                if (g != null && g.size_ > 1) {
                    writer.write(String.valueOf(id));
                    writer.write(",");
                    writer.write(String.valueOf(g.id_));
                    writer.write("\n");
                    ++numFiles_;
                }
            });
            System.out.println("    analyzed files:      " + total);
            System.out.println("    valid memberships:   " + numFiles_);
        } catch (IOException e) {
            e.printStackTrace();
        }
        // now let's create tokenHash, scan stats and unify all fileHashes that have same tokenHash
        tokenHashGroups_ = new HashMap<String, HashGroup>();
        filename = folder_ + "/" + Config.GENERIC_STATS + ".csv";
        total = CSVReader.file(filename, (ArrayList<String> row) -> {
            String fileHash = row.get(0);
            String tokenHash = row.get(7);
            HashGroup fg = hashGroups_.get(fileHash);
            HashGroup tg = tokenHashGroups_.get(tokenHash);
            if (tg == null) {
                tokenHashGroups_.put(tokenHash, fg);
            } else {
                if (fg != null)
                    tg.mergeWith(fg);
                hashGroups_.put(fileHash, tg);
            }
        });
        System.out.println("    analyzed stats:      " + total);
        System.out.println("    token hash groups:   " + tokenHashGroups_.size());
        numGroups_ = 0;
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder_ + "/" + Config.CLONE_GROUPS + "_tokenHash.csv"), "utf-8"))) {
            for (Map.Entry<String, HashGroup> e : tokenHashGroups_.entrySet()) {
                HashGroup g = e.getValue();
                if (g != null && g.size_ > 1) {
                    writer.write(String.valueOf(g.id_));
                    writer.write(",");
                    writer.write(String.valueOf(g.size_));
                    writer.write(",");
                    writer.write(String.valueOf(g.oldestId_));
                    writer.write(",");
                    writer.write(String.valueOf(g.fileHashes_));
                    writer.write("\n");
                    ++numGroups_;
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        System.out.println("    valid groups written:" + numGroups_);
        // write token hash membership
        numFiles_ = 0;
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder_ + "/" + Config.CLONE_PAIRS + "_tokenHash.csv"), "utf-8"))) {
            filename = folder_ + "/" + Config.FILES_ID_HASH_CREATED_AT + ".csv";
            total = CSVReader.file(filename, (ArrayList<String> row) -> {
                int id = Integer.parseInt(row.get(0));
                String hash = row.get(1);
                HashGroup g = hashGroups_.get(hash);
                if (g != null && g.size_ > 1) {
                    writer.write(String.valueOf(id));
                    writer.write(",");
                    writer.write(String.valueOf(g.id_));
                    writer.write("\n");
                    ++numFiles_;
                }
            });
            System.out.println("    analyzed files:      " + total);
            System.out.println("    valid memberships:   " + numFiles_);
        } catch (IOException e) {
            e.printStackTrace();
        }


    }





    private String folder_;

    private Map<Integer, CloneGroup> groups_;
    private Map<Integer, Set<CloneGroup>> groupMembers_;
    private int numGroups_;
    private int numFiles_;

    private Map<String, HashGroup> hashGroups_;
    private Map<String, HashGroup> tokenHashGroups_;
}
