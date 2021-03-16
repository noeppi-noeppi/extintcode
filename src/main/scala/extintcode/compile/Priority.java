package extintcode.compile;

public enum Priority {
    POWER(true),
    MULTIPLICATIVE(false),
    ADDITIVE(false),
    RELATIONAL(false),
    EQUALITY(false),
    LOGICAL_AND(false),
    LOGICAL_OR(false)
    ;
    
    public final boolean rightAssociative;

    Priority(boolean rightAssociative) {
        this.rightAssociative = rightAssociative;
    }
    
    public int priority() {
        return values().length - ordinal() - 1;
    }
}
