package cz.bbn.cerberus.document.dto;

import lombok.Getter;
import lombok.Setter;

import java.io.InputStream;

@Getter
@Setter
public class DocumentFileDto {

    private String name;
    private InputStream fileData;
}
