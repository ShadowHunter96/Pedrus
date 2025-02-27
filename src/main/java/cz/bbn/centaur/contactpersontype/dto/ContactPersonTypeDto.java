package cz.bbn.cerberus.contactpersontype.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;

@Getter
@Setter
@ToString
public class ContactPersonTypeDto implements Serializable {

    private String id;
    private String name;
    private Boolean allowed;
}
