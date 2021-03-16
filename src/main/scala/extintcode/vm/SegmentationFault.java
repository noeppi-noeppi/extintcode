package extintcode.vm;

public class SegmentationFault extends RuntimeException {
    
    private static final long serialVersionUID = -6440264434902273683L;
    
    public final long accessTry;
    public final int instruction;
    public final int relative;
    public final int section;
    public final long[] memory;

    public SegmentationFault(long accessTry, int instruction, int relative, int section, long[] memory, String message) {
        super(message);
        this.accessTry = accessTry;
        this.instruction = instruction;
        this.relative = relative;
        this.section = section;
        this.memory = memory;
    }
}
