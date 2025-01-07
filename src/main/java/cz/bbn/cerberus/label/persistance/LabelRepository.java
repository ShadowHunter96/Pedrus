package cz.bbn.cerberus.label.persistance;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface LabelRepository extends JpaRepository<LabelEntity, String>, JpaSpecificationExecutor<LabelEntity> {
}
