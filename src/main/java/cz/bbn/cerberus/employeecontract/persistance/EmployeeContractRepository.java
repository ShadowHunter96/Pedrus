package cz.bbn.cerberus.employeecontract.persistance;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface EmployeeContractRepository extends JpaRepository<EmployeeContractEntity, String>,
        JpaSpecificationExecutor<EmployeeContractEntity> {

    @Query("select entity.id from EmployeeContractEntity entity")
    List<String> getAllId();

    @Query("select entity.sequence from EmployeeContractEntity entity " +
            "where entity.sequence != null order by entity.sequence desc")
    List<Integer> getSequenceList();

    @Query("select entity.subsequence from EmployeeContractEntity entity " +
            "where entity.linkedContract = :linkedContractId " +
            "and entity.subsequence != null order by entity.subsequence desc")
    List<Integer> getSubSequence(String linkedContractId);

    @Query("select entity from EmployeeContractEntity entity " +
            "where entity.deleted != true and entity.archived != true and entity.id != :id")
    List<EmployeeContractEntity> findAllAllowedEmployeeContractListExceptId(String id);
}
