import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;

/**
 * Created by peta on 19.12.16.
 */
public class Helpers {
    public static void writeRow(ArrayList<String> row, Writer writer) throws IOException {
        writer.write(row.get(0));
        for (int i = 1; i < row.size(); ++i) {
            writer.write(",");
            writer.write(row.get(i));
        }
        writer.write("\n");
    }

    public static void writeFilesRow(ArrayList<String> row, Writer writer) throws IOException {
        writer.write(row.get(0)); // file id
        writer.write(",");
        writer.write(row.get(1)); // project id
        writer.write(",");
        writer.write(CSVReader.escape(row.get(2))); // relPath
        writer.write(",");
        writer.write(row.get(3)); // fileHash
        writer.write("\n");
    }
}
