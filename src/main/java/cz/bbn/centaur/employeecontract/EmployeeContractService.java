package cz.bbn.cerberus.employeecontract;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.custompermission.dto.CustomUserPermissionDto;
import cz.bbn.cerberus.employeecontract.dto.EmployeeContractDto;
import cz.bbn.cerberus.employeecontract.dto.EmployeeContractFilterDto;
import cz.bbn.cerberus.employeecontract.factory.EmployeeContractFactory;
import cz.bbn.cerberus.employeecontract.persistance.EmployeeContractDao;
import cz.bbn.cerberus.employeecontract.persistance.EmployeeContractEntity;
import cz.bbn.cerberus.employeecontract.persistance.EmployeeContractRepository;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;

@Service
public class EmployeeContractService {

    private final EmployeeContractDao employeeContractDao;
    private final EmployeeContractRepository employeeContractRepository;
    private final AppLogService appLogService;

    public EmployeeContractService(EmployeeContractDao employeeContractDao,
                                   EmployeeContractRepository employeeContractRepository, AppLogService appLogService) {
        this.employeeContractDao = employeeContractDao;
        this.employeeContractRepository = employeeContractRepository;
        this.appLogService = appLogService;
    }

    public Page<EmployeeContractDto> findContractDtoPage(EmployeeContractFilterDto filter) {
        return employeeContractDao.findEmployeeContractPage(filter);
    }

    public EmployeeContractDto getEmployeeContract(String id) throws SystemException {
        EmployeeContractEntity entity = getEntityById(id);
        return EmployeeContractFactory.fromEntity(entity);
    }

    private EmployeeContractEntity getEntityById(String id) throws SystemException {
        return employeeContractRepository.findById(id)
                .orElseThrow(() -> new SystemException(ErrorCode.EMPLOYEE_CONTRACT_NOT_EXISTS, id));
    }

    @Transactional
    public String saveEmployeeContract(EmployeeContractDto dto) throws SystemException {
        List<Integer> sequenceList = employeeContractRepository.getSequenceList();
        int sequence = 0;
        if (sequenceList.isEmpty() || sequenceList.get(0) == null) {
            sequence++;
        } else {
            sequence = sequenceList.get(0) + 1;
        }

        EmployeeContractEntity entity = new EmployeeContractEntity();

        String id = AppUtils.generateId("CE", sequence);

        if (employeeContractRepository.existsById(id)) {
            throw new SystemException(ErrorCode.CONTRACT_ALREADY_EXISTS, id);
        }

        if (dto.getType() != null && Boolean.TRUE.equals(dto.getType().getConnectionRequired())) {
            List<Integer> subSequenceList = employeeContractRepository.getSubSequence(dto.getLinkedContractId());
            int subsequence = 0;
            if (subSequenceList.isEmpty() || subSequenceList.get(0) == null) {
                subsequence++;
            } else {
                subsequence = subSequenceList.get(0) + 1;
            }
            dto.setSubsequence(subsequence);
            id = AppUtils.generateIdAddition(dto.getLinkedContractId(), subsequence);
        } else {
            dto.setSequence(sequence);
        }

        dto.setId(id);

        saveEmployeeContract(entity, dto);

        CustomUserPermissionDto customUserPermissionDto = new CustomUserPermissionDto(
                DomainEnum.CONTRACT_DOMAIN_NAME.getValue(), CustomPermissionService.ALL_PERMISSION,
                SecurityUtils.getCurrentUserId(), dto.getId(), true
        );
        appLogService.logInsert(customUserPermissionDto, CustomPermissionService.CUSTOM_PERMISSION_OBJECT_NAME);
        appLogService.logInsert(dto, DomainEnum.CONTRACT_DOMAIN_NAME.getValue());
        return id;
    }

    @Transactional
    public void updateEmployeeContract(EmployeeContractDto dto, EmployeeContractDto originalDto)
            throws SystemException {
        EmployeeContractEntity entity = getEntityById(dto.getId());
        saveEmployeeContract(entity, dto);
        appLogService.logUpdate(dto, originalDto, DomainEnum.EMPLOYEE_CONTRACT_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void deleteEmployeeContract(String id) throws SystemException {
        EmployeeContractEntity entity = getEntityById(id);
        entity.setDeleted(true);
        employeeContractRepository.save(entity);
        appLogService.logDelete(id, DomainEnum.EMPLOYEE_CONTRACT_DOMAIN_NAME.getValue());
    }

    private void saveEmployeeContract(EmployeeContractEntity entity, EmployeeContractDto dto) throws SystemException {
        if (dto.getLinkedContractId() != null) {
            if (dto.getLinkedContractId().equals(dto.getId())) {
                throw new SystemException(ErrorCode.ITSELF_CONNECT_ERROR, dto.getId());
            }
        }
        EmployeeContractFactory.fillEntity(entity, dto);
        employeeContractRepository.save(entity);
    }

    public List<EmployeeContractDto> getEmployeeContractListExceptId(String id) {
        if (id != null) {
            return ConvertEntities.fromEntities(
                    employeeContractRepository.findAllAllowedEmployeeContractListExceptId(id),
                    EmployeeContractFactory::fromEntity);
        } else {
            return employeeContractDao.findAllAllowedEmployeeContractList();
        }
    }
}
