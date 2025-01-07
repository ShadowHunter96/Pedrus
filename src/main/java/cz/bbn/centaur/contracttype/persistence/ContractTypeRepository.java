package cz.bbn.cerberus.contracttype.persistence;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface ContractTypeRepository extends JpaRepository<ContractTypeEntity, String> {

    @Query(" select entity " +
            "from ContractTypeEntity entity " +
            "where entity.allowed = true and entity.id != :exceptId")
    List<ContractTypeEntity> findAllEnabled(String exceptId);
}
