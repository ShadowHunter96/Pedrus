package cz.bbn.cerberus.api;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class FileRequest {

    private String name;
    private byte[] file;
}
