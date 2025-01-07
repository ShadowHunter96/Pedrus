package cz.bbn.cerberus.phase.dto;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class PhaseDto implements Serializable {

    private Long id;
    private String name;
    private String projectId;
}
