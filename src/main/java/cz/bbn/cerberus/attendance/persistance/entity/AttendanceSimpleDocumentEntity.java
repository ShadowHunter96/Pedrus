package cz.bbn.cerberus.attendance.persistance.entity;

import cz.bbn.cerberus.user.persistance.UserEntity;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.time.LocalDateTime;

@Entity
@Table(name = "attendance_document", schema = "backoffice")
@Getter
@Setter
// @TODO - jednoho dne po predelani funkcionality dokumentu bude lepsi, kdyz se tyto funkcionality spoji. Pak bude mozne tuto tabulku popripade sloupec file oddelat
public class AttendanceSimpleDocumentEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    private String name;
    private LocalDateTime date;

    @OneToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserEntity userEntity;

    private Boolean deleted;
}
