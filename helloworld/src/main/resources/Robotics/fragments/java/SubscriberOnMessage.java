package Robotics.fragments.java;

import basic_ros_application.msgs.StringMessage;
import org.apache.commons.logging.Log;

public class SubscriberOnMessage {

    public static void run(Log log, StringMessage message) {
        log.info("Subscriber - Received: [" + message.getData() + "]");
    }
}
