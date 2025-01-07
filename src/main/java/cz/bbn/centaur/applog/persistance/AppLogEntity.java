package cz.bbn.cerberus.applog.persistance;

import cz.bbn.cerberus.user.persistance.UserEntity;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.annotations.NotFound;
import org.hibernate.annotations.NotFoundAction;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.time.LocalDateTime;

@Entity
@Table(name = "app_log", schema = "security")
@Getter
@Setter
public class AppLogEntity {

    @Id
    private LocalDateTime date;

    private String action;

    @Column(name = "user_id")
    private Long userId;

    private String message;

    @Column(name = "object_id")
    private String appId;

    @OneToOne
    @NotFound(action = NotFoundAction.IGNORE)
    @JoinColumn(name = "user_id", referencedColumnName = "id", insertable = false, updatable = false)
    private UserEntity userEntity;
}
