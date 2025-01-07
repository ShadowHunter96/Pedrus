package cz.bbn.cerberus.tasktype.persistance;

import org.springframework.data.jpa.repository.JpaRepository;

public interface TaskTypeRoleRepository extends JpaRepository<TaskTypeRoleEntity, Long> {

    void deleteByIdTaskTypeEntityId(Long id);
}
