package cz.bbn.cerberus.tasktemplate.persistance.repository;

import cz.bbn.cerberus.tasktemplate.persistance.entity.TaskTemplateRoleEntity;
import cz.bbn.cerberus.tasktemplate.persistance.entity.TaskTemplateRoleId;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TaskTemplateRoleRepository extends JpaRepository<TaskTemplateRoleEntity, TaskTemplateRoleId> {

    void deleteByIdTaskTemplateEntityId(Long id);
}
