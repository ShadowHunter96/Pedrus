package cz.bbn.cerberus.schedulerlog.persistance;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.time.LocalDateTime;

@Table(name = "scheduler_log", schema = "security")
@Entity
@Getter
@Setter
public class SchedulerLogEntity {

    @Id
    private LocalDateTime date;

    private String description;

    private String exception;
}
