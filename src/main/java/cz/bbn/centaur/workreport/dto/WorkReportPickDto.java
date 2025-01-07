package cz.bbn.cerberus.workreport.dto;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class WorkReportPickDto implements Serializable {

    private String id;
    private String name;
}
