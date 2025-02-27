package cz.bbn.cerberus.dsmessage.dto;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;


@Getter
@Setter
public class DsMessageAttachementDto implements Serializable {
    private Long id;
    private Long messageId;
    private String name;
}
