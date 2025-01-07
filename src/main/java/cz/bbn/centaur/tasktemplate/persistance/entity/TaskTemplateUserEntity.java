package cz.bbn.cerberus.tasktemplate.persistance.entity;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "task_template_user", schema = "other")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class TaskTemplateUserEntity {

    @EmbeddedId
    private TaskTemplateUserId id;
}
