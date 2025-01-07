package cz.bbn.cerberus.tasktemplate.persistance.entity;

import cz.bbn.cerberus.user.persistance.UserEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Embeddable;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import java.io.Serializable;

@Embeddable
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class TaskTemplateUserId implements Serializable {

    @OneToOne
    @JoinColumn(name = "task_template_id", referencedColumnName = "id")
    private TaskTemplateEntity taskTemplateEntity;

    @OneToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    private UserEntity userEntity;
}
