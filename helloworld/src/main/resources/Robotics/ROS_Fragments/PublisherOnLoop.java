import basic_ros_application.msgs.StringMessage;
import org.apache.commons.logging.Log;
public class PublisherOnLoop {

    public static void run(Log log, StringMessage message) throws InterruptedException {

        Thread.sleep(1000);
        message.setData((System.currentTimeMillis()) + " milliseconds");
    }
}
