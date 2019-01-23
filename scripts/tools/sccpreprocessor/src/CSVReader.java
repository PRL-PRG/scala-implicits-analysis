import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

/**
 * Created by peta on 17.12.16.
 */


public class CSVReader {

    public static final char SEPARATOR = ',';

    public static final char QUOTE = '\"';

    public static final char ESCAPE = '\\';

    public interface RowHandler {
        void process(ArrayList<String> list) throws IOException;
    }

    /** Reads given CSV file and invokes the function passed in second argument on each line. Returns the number of rows parsed.
     */
    public static int file(String filename, RowHandler handler) {
        CSVReader r = new CSVReader(handler);
        r.parse(filename);
        return r.rows_;
    }

    /** Parses row from given buffered reader. Returns nullptr if end of file has been reached
     */
    public static ArrayList<String> parseRow(BufferedReader br) throws IOException {
        ArrayList row = new ArrayList();
        String line;
        while (true) {
            row.clear(); // we start new row
            line = br.readLine();
            if (line == null)
                return null;
            // no, we have line
            int i = 0;
            while (true) {
                String col = new String();
                if (i >= line.length()) {

                } else if (line.charAt(i) == QUOTE) {
                    ++i;
                    while (line.charAt(i) != QUOTE) {
                        if (line.charAt(i) == ESCAPE) {
                            ++i;
                            if (i == line.length()) {
                                line = br.readLine();
                                i = 0;
                                col += '\n';
                                continue;
                            }
                        }
                        col += line.charAt(i++);
                    }
                    ++i; // move past the ending quote
                } else {
                    while (i < line.length() && line.charAt(i) != SEPARATOR)
                        col += line.charAt(i++);
                }
                // add the column to the row
                row.add(col);
                if (i < line.length() && line.charAt(i++) == SEPARATOR)
                    continue;
                break;
            }
            return row;
        }

    }

    /** Escapes given text in quotes so that we can safely output it.
     */
    public static String escape(String what) {
        StringBuilder sb = new StringBuilder();
        sb.append(QUOTE);
        for (int i = 0; i < what.length(); ++i) {
            char x = what.charAt(i);
            switch (x) {
                case '\"':
                case '\\':
                case '\'':
                    sb.append(ESCAPE);
                    sb.append(x);
                    break;
                case '\n':
                    sb.append("\\n");
                    break;
                case '\t':
                    sb.append("\\t");
                    break;
                default:
                    sb.append(x);
            }
        }
        sb.append(QUOTE);
        return sb.toString();
    }

    private CSVReader(RowHandler handler) {
        rows_ = 0;
        handler_ = handler;
    }

    private void parse(String filename) {
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            while (true) {
                ArrayList<String> row = parseRow(br);
                if (row == null)
                    break;
                // we have parsed line, call handler
                handler_.process(row);
                // and increase rows
                ++rows_;
                if (rows_ % 10000000 == 0)
                    System.out.println(rows_);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }


    }


    private int rows_;
    private RowHandler handler_;

}

