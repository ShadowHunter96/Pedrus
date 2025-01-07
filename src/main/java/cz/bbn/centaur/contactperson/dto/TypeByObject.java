package cz.bbn.cerberus.contactperson.dto;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class TypeByObject implements Serializable {

    private String id;
    private ContactPersonObjectTypeEnum contactPersonObjectTypeEnum;

    public TypeByObject(String id, ContactPersonObjectTypeEnum contactPersonObjectTypeEnum) {
        this.id = id;
        this.contactPersonObjectTypeEnum = contactPersonObjectTypeEnum;
    }
}
