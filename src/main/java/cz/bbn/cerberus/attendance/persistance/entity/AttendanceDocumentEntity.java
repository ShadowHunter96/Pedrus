package cz.bbn.cerberus.attendance.persistance.entity;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import java.time.LocalDateTime;

@Entity
@Table(name = "attendance_document", schema = "backoffice")
@Getter
@Setter
public class AttendanceDocumentEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    private String name;
    private LocalDateTime date;

    @Column(name = "user_id")
    private Long userId;

    private Boolean deleted;

    private byte[] file;
}
