package cz.bbn.cerberus.contract.dto;

public enum ContractInternalType {
    SALES, SUPPLIER, OPERATIONAL, EMPTY;

    public static ContractInternalType getValueOrEmpty(String value) {
        for (ContractInternalType type : ContractInternalType.values()) {
            if (type.name().equals(value)) {
                return type;
            }
        }
        return EMPTY;
    }
}
