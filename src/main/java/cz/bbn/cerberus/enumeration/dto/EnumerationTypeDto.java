package cz.bbn.cerberus.enumeration.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;

@Getter
@Setter
@ToString
@NoArgsConstructor
public class EnumerationTypeDto implements Serializable {
    private String id;
    private String name;
    private String description;
    private String translationKey;
    private String permissionKey;
}
