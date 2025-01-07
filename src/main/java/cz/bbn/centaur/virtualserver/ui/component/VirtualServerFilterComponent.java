package cz.bbn.cerberus.virtualserver.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.IntegerField;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.project.ui.ProjectView;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerFilterDto;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerStatus;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.Map;
import java.util.Objects;

@Slf4j
public class VirtualServerFilterComponent extends FormLayout {

    private final Button search;
    private final String params;
    private final List<UserDto> userList;
    private final HistoryBreadcrumbs historyBreadcrumbs;
    private final boolean isInfrastructure;

    private IntegerField id;
    private TextField name;
    private TextField os;
    private IntegerField cpu;
    private IntegerField cores;
    private IntegerField ram;
    private TextField ip;
    private ComboBox<UserDto> owner;
    private ComboBox<VirtualServerStatus> status;
    private Checkbox showDeleted;

    public VirtualServerFilterComponent(Button search, List<UserDto> userList, String params,
                                        HistoryBreadcrumbs historyBreadcrumbs, boolean isInfrastructure) {
        this.params = params;
        this.search = search;
        this.userList = userList;
        this.historyBreadcrumbs = historyBreadcrumbs;
        this.isInfrastructure = isInfrastructure;
        initComponents();
    }

    private void initComponents() {

        id = new IntegerField(Transl.get("Id"));
        this.add(id);

        name = new TextField(Transl.get("Name"));
        this.add(name);

        os = new TextField(Transl.get("OS"));
        this.add(os);

        cpu = new IntegerField(Transl.get("CPU"));
        this.add(cpu);

        cores = new IntegerField(Transl.get("Cores"));
        this.add(cores);

        ram = new IntegerField(Transl.get("RAM"));
        this.add(ram);

        ip = new TextField(Transl.get("IP"));
        this.add(ip);

        owner = new ComboBox<>(Transl.get("Owner"));
        owner.setItems(userList);
        owner.setItemLabelGenerator(UserDto::getName);
        if (isInfrastructure) {
            this.add(owner);
        }

        status = new ComboBox<>(Transl.get("Status"));
        status.setItems(VirtualServerStatus.nonDeletedValues());
        status.setItemLabelGenerator(vsStat -> Transl.get(vsStat.getValue()));
        this.add(status);

        showDeleted = new Checkbox(Transl.get("Show deleted"));
        if (SecurityUtils.hasPermission(Permission.VIRTUAL_SERVER_SHOW_DELETED)) {
            this.add(showDeleted);
        }

        add(search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
        fromUrl();
    }

    private void fromUrl() {
        Map<String, String> map = AppUtils.getMapByParams(params);
        if (map.containsKey("id")) {
            try {
                id.setValue(Integer.valueOf(map.get("id")));
            } catch (NumberFormatException exception) {
                log.error(exception.getMessage(), exception);
            }
        }
        if (map.containsKey("name")) {
            name.setValue(map.get("name"));
        }
        if (map.containsKey("os")) {
            os.setValue(map.get("os"));
        }
        if (map.containsKey("cpu")) {
            cpu.setValue(Integer.valueOf(map.get("cpu")));
        }
        if (map.containsKey("cores")) {
            cores.setValue(Integer.valueOf(map.get("cores")));
        }
        if (map.containsKey("ram")) {
            ram.setValue(Integer.valueOf(map.get("ram")));
        }
        if (map.containsKey("ip")) {
            ip.setValue(map.get("ip"));
        }
        if (map.containsKey("owner") && isInfrastructure) {
            UserDto userDto = userList.stream().filter(actualUserDto ->
                    actualUserDto.getId().equals(Long.valueOf(map.get("userId")))).findAny().orElse(null);
            owner.setValue(userDto);
        }
        if (!isInfrastructure) {
            owner.setValue(SecurityUtils.getCurrentUserDto());
        }
        if (map.containsKey("status")) {
            VirtualServerStatus vsStatus = VirtualServerStatus.getFromNameOrNonExistent(map.get("status"));
            if (vsStatus != VirtualServerStatus.NON_EXISTENT) {
                status.setValue(vsStatus);
            }
        }
        if (map.containsKey("showDeleted") && SecurityUtils.hasPermission(Permission.VIRTUAL_SERVER_SHOW_DELETED)) {
            showDeleted.setValue("true".equalsIgnoreCase(map.get("showDeleted")));
        }
    }

    public void fillUrl() {
        String paramUrl = ProjectView.ROUTE.concat("/");
        if (id.getValue() != null && id.getValue() > 0) {
            paramUrl = paramUrl.concat("&id=").concat(String.valueOf(id.getValue()));
        }
        if (name.getValue() != null && !name.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&name=").concat(name.getValue());
        }
        if (os.getValue() != null && !os.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&os=").concat(os.getValue());
        }
        if (cpu.getValue() != null && cpu.getValue() > 0) {
            paramUrl = paramUrl.concat("&cpu=").concat(String.valueOf(cpu.getValue()));
        }
        if (cores.getValue() != null && cores.getValue() > 0) {
            paramUrl = paramUrl.concat("&cores=").concat(String.valueOf(cores.getValue()));
        }
        if (ram.getValue() != null && ram.getValue() > 0) {
            paramUrl = paramUrl.concat("&ram=").concat(String.valueOf(ram.getValue()));
        }
        if (ip.getValue() != null && StringUtils.isEmpty(ip.getValue())) {
            paramUrl = paramUrl.concat("&ip=").concat(ip.getValue());
        }
        if (owner.getValue() != null && isInfrastructure) {
            paramUrl = paramUrl.concat("&owner=").concat(getOwnerId(owner.getValue()));
        }
        if (status.getValue() != null && VirtualServerStatus.NON_EXISTENT != status.getValue()) {
            paramUrl = paramUrl.concat("&status=").concat(status.getValue().name());
        }
        if (showDeleted.getValue() != null && SecurityUtils.hasPermission(Permission.VIRTUAL_SERVER_SHOW_DELETED)) {
            paramUrl = paramUrl.concat("&showDeleted=").concat(showDeleted.getValue().toString());
        }

        UI.getCurrent().getPage().getHistory().pushState(null, paramUrl);
        historyBreadcrumbs.refreshLastBreadcrumb(paramUrl);
    }


    public VirtualServerFilterDto getVirtualServerFilterDto() {
        VirtualServerFilterDto dto = new VirtualServerFilterDto();
        dto.setId(id.getValue());
        dto.setName(name.getValue());
        dto.setOs(os.getValue());
        dto.setCpu(cpu.getValue());
        dto.setCores(cores.getValue());
        dto.setRam(ram.getValue());
        dto.setIp(ip.getValue());
        dto.setOwner(owner.getValue());
        dto.setStatus(status.getValue());
        dto.setShowDeleted(Boolean.TRUE.equals(showDeleted.getValue()));
        return dto;
    }

    private String getOwnerId(UserDto userDto) {
        for (UserDto user : userList) {
            if (Objects.equals(user.getId(), userDto.getId())) {
                return String.valueOf(user.getId());
            }
        }
        return "";
    }
}
