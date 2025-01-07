package cz.bbn.cerberus.tasktemplate.persistance.repository;

import cz.bbn.cerberus.tasktemplate.persistance.entity.TaskTemplateUserEntity;
import cz.bbn.cerberus.tasktemplate.persistance.entity.TaskTemplateUserId;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TaskTemplateUserRepository extends JpaRepository<TaskTemplateUserEntity, TaskTemplateUserId> {

    void deleteByIdTaskTemplateEntityId(Long id);
}
