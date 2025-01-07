package cz.bbn.cerberus.contactpersontype.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;

@Getter
@Setter
@ToString
public class ChangeContactPersonTypeDto implements Serializable {

    private ContactPersonTypeDto contactPersonTypeDto;
}
