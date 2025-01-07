package cz.bbn.cerberus.workreport.persistance.entity;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import java.io.Serializable;

@Embeddable
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class WorkReportClosedId implements Serializable {

    private String month;
    private String year;

    @Column(name = "employee_id")
    private String employeeId;
}
