package cz.bbn.cerberus.user.persistance;

import cz.bbn.cerberus.employee.persistance.entity.EmployeeEntity;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.util.Set;

@Entity
@Table(name = "user", schema = "security")
@Getter
@Setter
@NoArgsConstructor
public class UserEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "user_login")
    private String login;

    private String name;
    private String mail;
    private Boolean deleted;

    @Column(name = "send_unread_mails")
    private Boolean sendUnreadMails;

    @Column(name = "azure_id")
    private String azureId;

    @OneToOne
    @JoinColumn(name = "employee_id", referencedColumnName = "id")
    private EmployeeEntity employee;

    @OneToMany(fetch = FetchType.EAGER)
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private Set<UserActiveRoleEntity> userActiveRoleEntitySet;

    @Column(name = "preferred_language")
    private String preferredLanguage;

    @Column(name = "user_acronym")
    private String acronym;

    @Column(name = "user_roles")
    private String userRoles;

    public UserEntity(Long id) {
        this.id = id;
    }
}
