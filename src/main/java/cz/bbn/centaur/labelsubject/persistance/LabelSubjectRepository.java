package cz.bbn.cerberus.labelsubject.persistance;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface LabelSubjectRepository
        extends JpaRepository<LabelSubjectEntity, Long>, JpaSpecificationExecutor<LabelSubjectEntity> {

    @Query("select distinct subjectId from LabelSubjectEntity entity where entity.labelEntity.id = :id")
    List<String> findByLabelId(String id);

    @Modifying
    @Query("delete from LabelSubjectEntity entity where entity.labelEntity.id = :labelId")
    void deleteByLabelEntityId(String labelId);

    boolean existsBySubjectIdAndLabelEntityId(String subjectId, String labelId);
}
