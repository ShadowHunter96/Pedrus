package cz.bbn.cerberus.employee;

import com.vaadin.flow.component.datepicker.DatePicker;
import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.employee.dto.EmployeeByObjectDto;
import cz.bbn.cerberus.employee.dto.EmployeeByObjectFilterDto;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.employee.dto.EmployeeFilterDto;
import cz.bbn.cerberus.employee.dto.EmployeeLinkDto;
import cz.bbn.cerberus.employee.factory.EmployeeFactory;
import cz.bbn.cerberus.employee.persistance.EmployeeDao;
import cz.bbn.cerberus.employee.persistance.entity.EmployeeByObjectEntity;
import cz.bbn.cerberus.employee.persistance.entity.EmployeeByObjectId;
import cz.bbn.cerberus.employee.persistance.entity.EmployeeEntity;
import cz.bbn.cerberus.employee.persistance.repository.EmployeeByObjectRepository;
import cz.bbn.cerberus.employee.persistance.repository.EmployeeRepository;
import cz.bbn.cerberus.listconfiguration.ListService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

@Service
public class EmployeeService {

    private final EmployeeDao employeeDao;
    private final EmployeeRepository employeeRepository;
    private final EmployeeByObjectRepository employeeByObjectRepository;
    private final AppLogService appLogService;
    private final ListService listService;

    public EmployeeService(EmployeeDao employeeDao, EmployeeRepository employeeRepository,
                           EmployeeByObjectRepository employeeByObjectRepository, AppLogService appLogService,
                           ListService listService) {
        this.employeeDao = employeeDao;
        this.employeeRepository = employeeRepository;
        this.employeeByObjectRepository = employeeByObjectRepository;
        this.appLogService = appLogService;
        this.listService = listService;
    }

    public Page<EmployeeDto> findEmployeeDtoPage(EmployeeFilterDto filter) {
        return employeeDao.findEmployeePage(filter);
    }

    public Page<EmployeeByObjectDto> findEmployeeByObjectDtoPage(EmployeeByObjectFilterDto filter) {
        return employeeDao.findEmployeeByObjectPage(filter);
    }

    public List<String> getLinkedEmployeeIdList(String objectId, ObjectType objectType) {
        return employeeByObjectRepository.getLinkedEmployeeIdList(objectId, objectType);
    }

    @Transactional
    public void deleteEmployee(String id) throws SystemException {
        EmployeeEntity entity = getEntityById(id);
        entity.setDeleted(true);
        employeeRepository.save(entity);
        appLogService.logDelete(id, DomainEnum.EMPLOYEE_DOMAIN_NAME.getValue());
        listService.reloadEmployeeDtoList();
        listService.reloadUserDtoList();
    }

    @Transactional
    public void deleteEmployeeByObject(EmployeeByObjectId id) {
        employeeByObjectRepository.deleteById(id);
        appLogService.logDelete(id.toString(), "Employee by object");
    }

    private EmployeeEntity getEntityById(String id) throws SystemException {
        return employeeRepository.findById(id).orElseThrow(
                () -> new SystemException(ErrorCode.EMPLOYEE_DOES_NOT_EXISTS, id));
    }

    public EmployeeDto getEmployee(String id) throws SystemException {
        return EmployeeFactory.fromEntity(getEntityById(id));
    }

    @Transactional
    public void saveEmployee(EmployeeDto dto) throws SystemException {
        if (!employeeRepository.existsById(dto.getId())) {
            EmployeeEntity entity = new EmployeeEntity();
            saveEmployee(entity, dto);
            appLogService.logInsert(dto, DomainEnum.EMPLOYEE_DOMAIN_NAME.getValue());
            listService.reloadEmployeeDtoList();
            listService.reloadUserDtoList();
        } else {
            throw new SystemException(ErrorCode.EMPLOYEE_ALREADY_EXISTS, dto.getId());
        }
    }

    @Transactional
    public void updateEmployee(EmployeeDto newDto, EmployeeDto originalDto) throws SystemException {
        if (employeeRepository.existsById(newDto.getId())) {
            EmployeeEntity entity = getEntityById(newDto.getId());
            saveEmployee(entity, newDto);
            appLogService.logUpdate(newDto, originalDto, DomainEnum.EMPLOYEE_DOMAIN_NAME.getValue());
            listService.reloadEmployeeDtoList();
            listService.reloadUserDtoList();
        } else {
            throw new SystemException(ErrorCode.EMPLOYEE_DOES_NOT_EXISTS, newDto.getId());
        }
    }

    @Transactional
    public void saveEmployeeByObject(EmployeeLinkDto dto) throws SystemException {
        dto.getEmployeeDtoSet().forEach(employeeDto -> {
            EmployeeByObjectId employeeByObjectId = new EmployeeByObjectId();
            employeeByObjectId.setObjectId(dto.getObjectId());
            employeeByObjectId.setObjectType(dto.getObjectType());
            employeeByObjectId.setEmployeeId(employeeDto.getId());
            employeeByObjectRepository.save(new EmployeeByObjectEntity(employeeByObjectId));
            appLogService.log("link employee to project", "", employeeByObjectId.toString());
        });
    }

    private void saveEmployee(EmployeeEntity entity, EmployeeDto dto) {
        dto.setDeleted(Boolean.TRUE.equals(dto.getDeleted()));
        EmployeeFactory.fillEntity(entity, dto);
        employeeRepository.save(entity);
    }

    public List<EmployeeDto> findValidEmployeeList() {
        return ConvertEntities.fromEntities(employeeRepository.findValidList(), EmployeeFactory::fromEntity);
    }

    public String getEmployeeName(String employeeId) {
        Map<String, EmployeeDto> employeeMap = getEmployeeMap();
        if (employeeId != null && employeeMap.containsKey(employeeId)) {
            EmployeeDto employeeDto = employeeMap.get(employeeId);
            if (employeeDto.getFirstName() != null && !StringUtils.isEmpty(employeeDto.getFirstName()) &&
                    employeeDto.getLastName() != null && !StringUtils.isEmpty(employeeDto.getLastName())) {
                return employeeDto.getFirstName() + " " + employeeDto.getLastName();
            }
            return employeeDto.getId();
        }
        return "";
    }

    public String getEmployeeName(EmployeeDto employeeDto) {
        if (employeeDto != null) {
            String employeeName = employeeDto.getId();
            if (employeeDto.getFirstName() != null && !StringUtils.isEmpty(employeeDto.getFirstName()) &&
                    employeeDto.getLastName() != null && !StringUtils.isEmpty(employeeDto.getLastName())) {
                employeeName = employeeDto.getFirstName() + " " + employeeDto.getLastName();
            }
            if (employeeDto.getFirstName() != null && !StringUtils.isEmpty(employeeDto.getFirstName())) {
                employeeName = employeeDto.getFirstName();
            }
            if (employeeDto.getLastName() != null && !StringUtils.isEmpty(employeeDto.getLastName())) {
                employeeName = employeeDto.getLastName();
            }
            return employeeName;
        }
        return "";
    }

    private Map<String, EmployeeDto> getEmployeeMap() {
        Map<String, EmployeeDto> employeeMap = new HashMap<>();
        List<EmployeeDto> employeeList = findValidEmployeeList();
        for (EmployeeDto employeeDto : employeeList) {
            employeeMap.put(employeeDto.getId(), employeeDto);
        }
        return employeeMap;
    }

    public List<String> getObjectIdByEmployeeAndType(String employeeId, List<ObjectType> objectTypeList) {
        return employeeByObjectRepository.getObjectIdByEmployeeAndType(employeeId, objectTypeList);
    }

    public Long getEmployeeCountFromTo(DatePicker from, DatePicker to) {
        return employeeRepository.findEmployeeCountByPeriod(from.getValue(), to.getValue());
    }

    public Set<String> getEmployeeIdSetFromTo(DatePicker from, DatePicker to) {
        return employeeRepository.findEmployeeIdSetByPeriod(from.getValue(), to.getValue());
    }
}
