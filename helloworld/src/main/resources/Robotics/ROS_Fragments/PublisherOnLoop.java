import basic_ros_application.msgs.StringMessage;

public class PublisherOnLoop {

    public static void run(Log log, StringMessage message) throws InterruptedException {

        Thread.sleep(1000);
        message.setData((System.currentTimeMillis()) + " milliseconds");
    }
}
