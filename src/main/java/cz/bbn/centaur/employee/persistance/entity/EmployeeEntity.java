package cz.bbn.cerberus.employee.persistance.entity;

import cz.bbn.cerberus.user.persistance.UserEntity;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.time.LocalDate;

@Entity
@Table(name = "employee", schema = "backoffice")
@Getter
@Setter
public class EmployeeEntity {

    @Id
    private String id;

    @Column(name = "first_name")
    private String firstName;

    @Column(name = "last_name")
    private String lastName;

    @Column(name = "company_email")
    private String companyEmail;

    @Column(name = "personal_email")
    private String personalEmail;

    @Column(name = "company_phone_number")
    private String companyPhoneNumber;

    @Column(name = "personal_phone_number")
    private String personalPhoneNumber;

    @Column(name = "account_number")
    private String accountNumber;

    private String position;

    @Column(name = "start_date")
    private LocalDate startDate;

    private Boolean active;

    @Column(name = "dismiss_date")
    private LocalDate dismissDate;

    @OneToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "line_manager_user_id", referencedColumnName = "id")
    private UserEntity lineManagerUserEntity;

    @OneToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "superior_user_id", referencedColumnName = "id")
    private UserEntity superiorUserEntity;

    private Boolean deleted;

}
