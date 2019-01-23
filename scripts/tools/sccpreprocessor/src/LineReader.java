import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

/**
 * Created by peta on 19.12.16.
 */
public class LineReader {

    public interface LineHandler {
        void process(String line) throws IOException;
    }

    public static int file(String filename, LineHandler handler) {
        LineReader r = new LineReader(handler);
        r.parse(filename);
        return r.rows_;
    }

    private LineReader(LineHandler handler) {
        rows_ = 0;
        handler_  = handler;
    }

    private void parse(String filename) {
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while (true) {
                line = br.readLine();
                if (line == null)
                    break;
                // no, we have line
                handler_.process(line);
                // and increase rows
                ++rows_;
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private int rows_;
    private LineHandler handler_;
}
