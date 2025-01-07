package cz.bbn.cerberus.translation.dto;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TranslationDto {

    private Long id;
    private String lang;
    private String key;
    private String value;

}
