package extintcode.vm;

// This one is missing pointer info.
public class PartialSegmentationFault extends RuntimeException {

    private static final long serialVersionUID = 6684538553323838307L;
    
    public final long accessTry;

    public PartialSegmentationFault(long accessTry, String message) {
        super(message);
        this.accessTry = accessTry;
    }
}
