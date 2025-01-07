package cz.bbn.cerberus.approvement.persistance.entity;

import cz.bbn.cerberus.approvement.enums.ApprovementState;
import cz.bbn.cerberus.approvement.enums.ApprovementType;
import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationEntity;
import cz.bbn.cerberus.role.persistance.entity.RoleEntity;
import cz.bbn.cerberus.user.persistance.UserEntity;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "approvement", schema = "intranet")
@Getter
@Setter
public class ApprovementEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Enumerated(EnumType.STRING)
    @Column(name = "approvement_type")
    private ApprovementType approvementType;

    @Column(name = "date_from")
    private LocalDate dateFrom;

    @Column(name = "date_to")
    private LocalDate dateTo;

    @Column(name = "note")
    private String note;

    @OneToMany(fetch = FetchType.LAZY)
    @JoinColumn(name = "approvement_id", referencedColumnName = "id")
    private List<ApprovementProjectEmployeeEntity> approvementProjectEmployeeEntity;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "line_manager_user_id", referencedColumnName = "id")
    private UserEntity lineManagerUserEntity;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "line_manager_role", referencedColumnName = "id")
    private RoleEntity lineManagerRoleEntity;

    @Column(name = "line_manager_approved")
    private Boolean lineManageApproved;

    @Column(name = "line_manager_note")
    private String lineManageNote;

    @Column(name = "line_manager_approved_date")
    private LocalDateTime lineManageApprovedDate;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "superior_user_id", referencedColumnName = "id")
    private UserEntity superiorUserEntity;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "superior_role", referencedColumnName = "id")
    private RoleEntity superiorRoleEntity;

    @Column(name = "superior_approved")
    private Boolean superiorApproved;

    @Column(name = "superior_note")
    private String superiorNote;

    @Column(name = "superior_approved_date")
    private LocalDateTime superiorApprovedDate;

    @Column(name = "created")
    private LocalDateTime created;

    @OneToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "owner_user_id", referencedColumnName = "id")
    private UserEntity ownerUserEntity;

    @Enumerated(EnumType.STRING)
    @Column(name = "state")
    private ApprovementState approvementState;

    @Column(name = "half_day")
    private Boolean halfDay;

    private Double days;

    @OneToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "business_trip_id", referencedColumnName = "id")
    private ApprovementBusinessTripEntity approvementBusinessTripEntity;

    @OneToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "enumeration_id", referencedColumnName = "id")
    private EnumerationEntity enumeration;

    @OneToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "created_user_id", referencedColumnName = "id")
    private UserEntity createdUserEntity;

}
