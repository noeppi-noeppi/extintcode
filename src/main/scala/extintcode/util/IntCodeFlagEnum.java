package extintcode.util;

public enum IntCodeFlagEnum {
    
    ASCII(IntCodeFlags.ASCII()),
    DEBUG(IntCodeFlags.DEBUG()),
    EXD(IntCodeFlags.EXD());

    public final long flag;

    IntCodeFlagEnum(long flag) {
        this.flag = flag;
    }
    
    @Override
    public String toString() {
        return name().toLowerCase();
    }
}
