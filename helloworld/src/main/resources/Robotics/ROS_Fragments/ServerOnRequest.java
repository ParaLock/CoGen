import basic_ros_application.msgs.SumRequest;
import basic_ros_application.msgs.SumResponse;
import org.apache.commons.logging.Log;

public class ServerOnRequest {
    public static void run(Log log, SumRequest request, SumResponse response) {

        long sum = request.getA() + request.getB();

        response.setSum(sum);

        if (log != null) {
            log.info("Server: "+request.getA() + " + " + request.getB() + " = " + response.getSum());
        }
    }
}
