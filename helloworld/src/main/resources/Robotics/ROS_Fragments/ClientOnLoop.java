import basic_ros_application.msgs.SumRequest;
import org.apache.commons.logging.Log;

public class ClientOnLoop {

    public static void run(
            SumRequest request,
            Log log
    ) throws InterruptedException {

        Thread.sleep(1000);

        request.setA(1);
        request.setB(2);
        log.info("Client: How much is the sum " + request.getA() +"+"+request.getB()+"?");
    }
}
