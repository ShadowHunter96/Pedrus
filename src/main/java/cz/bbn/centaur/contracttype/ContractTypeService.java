package cz.bbn.cerberus.contracttype;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.contracttype.factory.ContractTypeFactory;
import cz.bbn.cerberus.contracttype.persistence.ContractTypeEntity;
import cz.bbn.cerberus.contracttype.persistence.ContractTypeRepository;
import cz.bbn.cerberus.listconfiguration.ListService;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;

@Service
public class ContractTypeService {

    private final ContractTypeRepository contractTypeRepository;
    private final ListService listService;
    private final AppLogService appLogService;

    public ContractTypeService(ContractTypeRepository contractTypeRepository, ListService listService,
                               AppLogService appLogService) {
        this.contractTypeRepository = contractTypeRepository;
        this.listService = listService;
        this.appLogService = appLogService;
    }

    public ContractTypeDto getContractType(String id) throws SystemException {
        ContractTypeEntity entity = getContractTypeEntity(id);
        return ContractTypeFactory.fromEntity(entity);
    }

    public Page<ContractTypeDto> findContractTypeDtoPage(int page, int size, List<Sort.Order> sortList) {
        Page<ContractTypeEntity> pageEntity = contractTypeRepository.findAll(
                PageRequest.of(page, size, Sort.by(sortList)));
        List<ContractTypeDto> list = ConvertEntities
                .fromEntities(pageEntity.toList(), ContractTypeFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(page,
                size, Sort.by(sortList)),
                pageEntity.getTotalElements());
    }

    public boolean contractTypeExists(String id) {
        return contractTypeRepository.existsById(id);
    }

    @Transactional
    public void saveContractType(ContractTypeDto dto) throws SystemException {
        if (contractTypeExists(dto.getId())) {
            throw new SystemException(ErrorCode.ID_ALREADY_EXISTS, dto.getId());
        }
        ContractTypeEntity entity = new ContractTypeEntity();
        saveContractType(entity, dto);
        appLogService.logInsert(dto, DomainEnum.CONTRACT_TYPE_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void updateContractType(ContractTypeDto dto, ContractTypeDto originalDto) throws SystemException {
        ContractTypeEntity entity = getContractTypeEntity(dto.getId());
        this.saveContractType(entity, dto);
        appLogService.logUpdate(dto, originalDto, DomainEnum.CONTRACT_TYPE_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void deleteContractType(String id) throws SystemException {
        if (!contractTypeExists(id)) {
            throw new SystemException(ErrorCode.CONTRACT_TYPE_NOT_EXISTS, id);
        }
        contractTypeRepository.deleteById(id);
        listService.reloadContractTypeDtoList();
        appLogService.logDelete(id, DomainEnum.CONTRACT_TYPE_DOMAIN_NAME.getValue());
    }

    private void saveContractType(ContractTypeEntity entity, ContractTypeDto dto) {
        ContractTypeFactory.fillEntity(entity, dto);
        contractTypeRepository.save(entity);
        listService.reloadContractTypeDtoList();
    }

    private ContractTypeEntity getContractTypeEntity(String id) throws SystemException {
        return contractTypeRepository.findById(id).orElseThrow(
                () -> new SystemException(ErrorCode.CONTRACT_TYPE_NOT_EXISTS, id));
    }

    public List<ContractTypeDto> findAllEnabled(String exceptId) {
        List<ContractTypeEntity> entityList = contractTypeRepository.findAllEnabled(exceptId);
        return ConvertEntities
                .fromEntities(entityList, ContractTypeFactory::fromEntity);
    }
}
