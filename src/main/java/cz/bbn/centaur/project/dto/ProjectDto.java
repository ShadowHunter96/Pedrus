package cz.bbn.cerberus.project.dto;

import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.project.ProjectState;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;
import java.time.LocalDate;

@Getter
@Setter
@ToString
public class ProjectDto implements Serializable {

    private String id;
    private String name;
    private String description;
    private SubjectDto subject;
    private ContractDto contract;
    private boolean deleted;
    private LocalDate startTime;
    private LocalDate endTime;
    private String color;
    private UserDto userDto;
    private ProjectState projectState;

    public String getColor() {
        if (color == null) {
            return ProjectColorEnum.BLUE.getCode();
        }
        return color;
    }

    public ProjectColorEnum getColorEnum() {
        return ProjectColorEnum.getById(color);
    }

    public void setColorEnum(ProjectColorEnum colorEnum) {
        color = colorEnum.getCode();
    }
}
