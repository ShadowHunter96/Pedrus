package cz.bbn.cerberus.ares;

import cz.bbn.cerberus.adis.AdisAnswer;
import cz.bbn.cerberus.adis.AdisItem;
import cz.bbn.cerberus.adis.AdisService;
import cz.bbn.cerberus.ares.dto.LawForm;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.ico.dto.IcoDto;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.tomcat.util.json.JSONParser;
import org.apache.tomcat.util.json.ParseException;
import org.springframework.stereotype.Service;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;


@Service
@Slf4j
public class AresService {

    private final AdisService adisService;
    private final AppEnv appEnv;

    public AresService(AppEnv appEnv, AdisService adisService) {
        this.appEnv = appEnv;
        this.adisService = adisService;
    }

    public IcoDto getDataFromAres(IcoDto icoDto) throws SystemException {
        JSONParser parser = new JSONParser(getAnswerVr(icoDto.getIco()));
        try {
            Map<String, Object> parsed = (Map<String, Object>) parser.parse();
            if (parsed.containsKey("ekonomickeSubjekty")) {
                List<Map<String, Object>> economicSubjects =
                        (List<Map<String, Object>>) parsed.get("ekonomickeSubjekty");
                if (!economicSubjects.isEmpty()) {
                    Map<String, Object> entry = economicSubjects.get(0);
                    fillVrData(entry, icoDto);
                }
            }
        } catch (ParseException e) {
            e.printStackTrace();
            throw new SystemException(ErrorCode.ARES_ANSWER_ERROR, e);
        }
        return icoDto;
    }

    public List<IcoDto> getIcoList(String ico, String name) throws SystemException {
        if (ico != null && !ico.trim().isEmpty()) {
            while (ico.length() < 8) {
                ico = "0".concat(ico);
            }
        }
        String esText = getAnswerEs(ico, name);
        List<IcoDto> icoDtoList = new ArrayList<>();
        List<String> dicList = new ArrayList<>();
        JSONParser parser = new JSONParser(esText);
        try {
            Map<String, Object> parsed = (Map<String, Object>) parser.parse();
            if (parsed.containsKey("kod") && parsed.containsKey("popis")) {
                ErrorNotification.show(String.valueOf(parsed.get("popis")), appEnv);
                return new ArrayList<>();
            }
            if (parsed.containsKey("ekonomickeSubjekty")) {
                List<Map<String, Object>> economicSubjectList = (List) parsed.get("ekonomickeSubjekty");
                fillIcoFromAresAdis(icoDtoList, dicList, economicSubjectList);
            }
        } catch (ParseException e) {
            e.printStackTrace();
            throw new SystemException(ErrorCode.ARES_ANSWER_ERROR, e);
        }
        return icoDtoList;
    }

    public IcoDto getAnswerFromEs(IcoDto icoDto) throws SystemException {
        String esText = getAnswerEs(icoDto.getIco(), "");
        JSONParser parser = new JSONParser(esText);
        try {
            Map<String, Object> parsed = (Map<String, Object>) parser.parse();
            if (parsed.containsKey("ekonomickeSubjekty")) {
                List<Map<String, Object>> economicSubjectList = (List) parsed.get("ekonomickeSubjekty");
                if (!economicSubjectList.isEmpty()) {
                    Map<String, Object> economicSubject = economicSubjectList.get(0);
                    fillEsData(economicSubject, icoDto);
                }
            }
        } catch (ParseException e) {
            e.printStackTrace();
            throw new SystemException(ErrorCode.ARES_ANSWER_ERROR, e);
        }
        return icoDto;
    }

    public IcoDto getSingleResponseFromAdis(IcoDto icoDto) {
        if (icoDto.getDic() != null && !"".equals(icoDto.getDic())) {
            String parsedDic = icoDto.getDic().replace("CZ", "");
            AdisAnswer adisResponse = adisService.getDataFromAdisByDic(List.of(parsedDic));
            if (adisResponse.getItemMap().containsKey(parsedDic)) {
                AdisItem adisItem = adisResponse.getItemMap().get(parsedDic);
                icoDto.setReliable(adisItem.getReliable());
                icoDto.setUnreliableFrom(adisItem.getUnreliableFrom());
                icoDto.setStandardAccount(adisItem.getStandardAccount());
                icoDto.setNonStandardAccount(adisItem.getNonStandardAccount());
            }
        }
        return icoDto;
    }

    private String getAnswerVr(String ico) throws SystemException {
        try {
            URL url = new URL(appEnv.getJusticeWso2Url() + "/vyhledat");
            HttpURLConnection con = (HttpURLConnection) url.openConnection();
            con.setRequestMethod("POST");
            con.setRequestProperty("Content-Type", "application/json");
            con.setDoOutput(true);
            String body = "{\"start\":0, \"pocet\": 1";
            if (ico != null && !ico.trim().isEmpty()) {
                body = body + ",\"ico\":[\"" + ico.trim() + "\"]";
            }
            body = body + "}";
            OutputStream os = con.getOutputStream();
            byte[] input = body.getBytes("utf-8");
            os.write(input, 0, input.length);
            os.close();
            BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream(), "utf-8"));
            String inputLine;
            StringBuilder content = new StringBuilder();
            while ((inputLine = in.readLine()) != null) {
                content.append(inputLine);
            }
            in.close();
            con.disconnect();
            return content.toString();
        } catch (IOException ex) {
            log.error(ex.getMessage(), ex);
            throw new SystemException(ErrorCode.ARES_ANSWER_ERROR, ex);
        }
    }

    private String getAnswerEs(String ico, String name) throws SystemException {
        try {
            URL url = new URL(appEnv.getAdisUrl() + "/vyhledat");
            HttpURLConnection con = (HttpURLConnection) url.openConnection();
            con.setRequestMethod("POST");
            con.setRequestProperty("Content-Type", "application/json");
            con.setDoOutput(true);
            String body = "{\"start\":0, \"pocet\": 1000";
            if (ico != null && !ico.trim().isEmpty()) {
                body = body + ",\"ico\":[\"" + ico.trim() + "\"]";
            }
            if (ico != null && !ico.trim().isEmpty() && name != null && !name.trim().isEmpty()) {
                body = body + ",";
            }
            if (name != null && !name.trim().isEmpty()) {
                body = body + ",\"obchodniJmeno\":\"" + name.trim() + "\"";
            }
            body = body + "}";
            OutputStream os = con.getOutputStream();
            byte[] input = body.getBytes("utf-8");
            os.write(input, 0, input.length);
            os.close();
            con.getResponseCode();
            InputStream stream = con.getErrorStream();
            if (stream == null) {
                stream = con.getInputStream();
            }
            BufferedReader in = new BufferedReader(new InputStreamReader(stream, "utf-8"));
            String inputLine;
            StringBuilder content = new StringBuilder();
            while ((inputLine = in.readLine()) != null) {
                content.append(inputLine);
            }
            in.close();
            con.disconnect();
            return content.toString();
        } catch (IOException ex) {
            log.error(ex.getMessage(), ex);
            throw new SystemException(ErrorCode.ARES_ANSWER_ERROR, ex);
        }
    }

    private void fillIcoFromAresAdis(List<IcoDto> icoDtoList, List<String> dicList,
                                     List<Map<String, Object>> economicSubjectList) {
        for (Map<String, Object> economicSubject : economicSubjectList) {
            if (economicSubject.containsKey("dic")) {
                dicList.add((String) economicSubject.get("dic"));
            }
        }
        AdisAnswer adisResponse = adisService.getDataFromAdisByDic(dicList);
        for (Map<String, Object> economicSubject : economicSubjectList) {
            IcoDto icoDto = new IcoDto();
            fillEsData(economicSubject, icoDto);
            if (icoDto.getDic() != null && !"".equals(icoDto.getDic().strip())) {
                String dicStripped = icoDto.getDic().replace("CZ", "");
                if (adisResponse.getItemMap().containsKey(dicStripped)) {
                    AdisItem adisItem = adisResponse.getItemMap().get(dicStripped);
                    icoDto.setReliable(adisItem.getReliable());
                    icoDto.setUnreliableFrom(adisItem.getUnreliableFrom());
                    icoDto.setStandardAccount(adisItem.getStandardAccount());
                    icoDto.setNonStandardAccount(adisItem.getNonStandardAccount());
                }
            }

            icoDtoList.add(icoDto);
        }
    }

    private void fillEsData(Map<String, Object> economicSubject, IcoDto icoDto) {
        if (economicSubject.containsKey("ico")) {
            icoDto.setIco((String) economicSubject.get("ico"));
        }
        if (economicSubject.containsKey("dic")) {
            icoDto.setDic("CZ" + economicSubject.get("dic"));
        }
        if (economicSubject.containsKey("obchodniJmeno")) {
            icoDto.setCompanyName((String) economicSubject.get("obchodniJmeno"));
        }
        if (economicSubject.containsKey("sidlo")) {
            Map<String, Object> addressMap = (Map<String, Object>) economicSubject.get("sidlo");
            fillAddress(addressMap, icoDto);
        }
        if (economicSubject.containsKey("pravniForma")) {
            icoDto.setLawForm(economicSubject.get("pravniForma") + " - " +
                    LawForm.LAW_FORM_MAP.get(String.valueOf(economicSubject.get("pravniForma"))));
        }
        if (economicSubject.containsKey("datumVzniku")) {
            icoDto.setEnlistDate(formatAresDate(String.valueOf(economicSubject.get("datumVzniku"))));
        }
        if ((icoDto.getDic() == null || icoDto.getDic().isEmpty()) && icoDto.getLawForm() != null
                && !icoDto.getLawForm().startsWith("1") && !icoDto.getLawForm().startsWith("2")) {
            icoDto.setDic("CZ" + icoDto.getIco());
        }
    }

    private void fillAddress(Map<String, Object> addressMap, IcoDto icoDto) {
        StringBuilder address = new StringBuilder();
        if (addressMap.containsKey("nazevUlice")) {
            address.append(addressMap.get("nazevUlice"));
        }
        if (addressMap.containsKey("cisloDomovni") && addressMap.containsKey("cisloOrientacni")) {
            address.append(" ").append(addressMap.get("cisloDomovni")).append("/")
                    .append(addressMap.get("cisloOrientacni"));
        } else if (addressMap.containsKey("cisloDomovni")) {
            address.append(" ").append(addressMap.get("cisloDomovni"));
        } else if (addressMap.containsKey("cisloOrientacni")) {
            address.append(" ").append(addressMap.get("cisloOrientacni"));
        }
        if (addressMap.containsKey("psc")) {
            address.append(", ").append(addressMap.get("psc"));
        }
        if (addressMap.containsKey("nazevObce")) {
            address.append(" ").append(addressMap.get("nazevObce"));
        }
        if (addressMap.containsKey("nazevCastiObce")) {
            address.append(" - ").append(addressMap.get("nazevCastiObce"));
        }
        if (addressMap.containsKey("nazevStatu")) {
            address.append(", ").append(addressMap.get("nazevStatu"));
        }
        icoDto.setAddress(address.toString());
    }

    private String formatAresDate(String oldFormat) {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate newFormat = LocalDate.parse(oldFormat, formatter);
        return AppUtils.formatDate(newFormat);
    }

    private void parseMemberName(Map<String, Object> member, List<String> allMembers) {
        StringBuilder memberName = new StringBuilder();
        if (member.containsKey("titulPredJmenem")) {
            memberName.append(member.get("titulPredJmenem")).append(" ");
        }
        if (member.containsKey("jmeno")) {
            memberName.append(member.get("jmeno")).append(" ");
        }
        if (member.containsKey("prijmeni")) {
            memberName.append(member.get("prijmeni"));
        }
        if (member.containsKey("titulZaJmenem")) {
            memberName.append(" ").append(member.get("titulZaJmenem"));
        }
        allMembers.add(memberName.toString());
    }

    private void fillVrData(Map<String, Object> entry, IcoDto icoDto) {
        if (entry.containsKey("zaznamy")) {
            List<Map<String, Object>> vrSubjectList = (List) entry.get("zaznamy");
            if (!vrSubjectList.isEmpty()) {
                Map<String, Object> vrSubject = vrSubjectList.get(0);
                fillVrCapital(vrSubject, icoDto);
                fillVrAddress(vrSubject, icoDto);
                if (vrSubject.containsKey("rejstrik")) {
                    icoDto.setRegister((String) vrSubject.get("rejstrik"));
                }
                fillVrCourtData(vrSubject, icoDto);
                fillVrLegalForm(vrSubject, icoDto);
                fillStatutoryOrgans(vrSubject, icoDto);
                fillCompanyName(vrSubject, icoDto);
            }
        }
    }

    private void fillVrCapital(Map<String, Object> vrSubject, IcoDto icoDto) {
        if (vrSubject.containsKey("zakladniKapital")) {
            List<Map<String, Object>> capitalList = (List<Map<String, Object>>) vrSubject.get("zakladniKapital");
            double capitalTotal = 0;
            for (Map<String, Object> capital : capitalList) {
                if (!(capital.containsKey("datumZapisu") && capital.containsKey("datumVymazu"))
                        && capital.containsKey("vklad")) {
                    Map<String, Object> deposit = (Map<String, Object>) capital.get("vklad");
                    if (deposit.containsKey("hodnota")) {
                        String depositParsed = (String) deposit.get("hodnota");
                        depositParsed = depositParsed.replace(";", ".");
                        capitalTotal = capitalTotal + Double.parseDouble(depositParsed);
                    }
                }
            }
            icoDto.setCapital(String.format("%.2f", capitalTotal));
        }
    }

    private void fillVrAddress(Map<String, Object> vrSubject, IcoDto icoDto) {
        if (vrSubject.containsKey("adresy")) {
            List<Map<String, Object>> addressList = (List<Map<String, Object>>) vrSubject.get("adresy");
            for (Map<String, Object> address : addressList) {
                if (!(address.containsKey("datumZapisu") && address.containsKey("datumVymazu"))
                        && address.containsKey("adresa")) {
                    fillAddress((Map<String, Object>) address.get("adresa"), icoDto);
                }
            }
        }
    }

    private void fillVrCourtData(Map<String, Object> vrSubject, IcoDto icoDto) {
        if (vrSubject.containsKey("spisovaZnacka")) {
            List<Map<String, Object>> courtInfoList = (List<Map<String, Object>>) vrSubject.get("spisovaZnacka");
            for (Map<String, Object> courtInfo : courtInfoList) {
                fillVrCourtInnerData(courtInfo, icoDto);
            }
        }
    }

    private void fillVrCourtInnerData(Map<String, Object> courtInfo, IcoDto icoDto) {
        if (!(courtInfo.containsKey("datumZapisu") && courtInfo.containsKey("datumVymazu"))) {
            if (courtInfo.containsKey("datumZapisu")) {
                icoDto.setEnlistDate(formatAresDate((String) courtInfo.get("datumZapisu")));
            }
            if (courtInfo.containsKey("soud")) {
                icoDto.setCourt((String) courtInfo.get("soud"));
            }
            if (courtInfo.containsKey("oddil") && courtInfo.containsKey("vlozka")) {
                icoDto.setFileNumber(courtInfo.get("oddil") + " " + courtInfo.get("vlozka"));
            }
        }
    }

    private void fillVrLegalForm(Map<String, Object> vrSubject, IcoDto icoDto) {
        if (vrSubject.containsKey("pravniForma")) {
            List<Map<String, Object>> lawFormList = (List<Map<String, Object>>) vrSubject.get("pravniForma");
            for (Map<String, Object> lawForm : lawFormList) {
                if (!(lawForm.containsKey("datumZapisu") && lawForm.containsKey("datumVymazu"))
                        && lawForm.containsKey("hodnota")) {
                    icoDto.setLawForm(lawForm.get("hodnota") + " - "
                            + LawForm.LAW_FORM_MAP.get(lawForm.get("hodnota")));
                }
            }
        }
    }

    private void fillStatutoryOrgans(Map<String, Object> vrSubject, IcoDto icoDto) {
        if (vrSubject.containsKey("statutarniOrgany")) {
            List<Map<String, Object>> organList = (List<Map<String, Object>>) vrSubject.get("statutarniOrgany");
            List<String> allOrgans = new ArrayList<>();
            for (Map<String, Object> organ : organList) {
                if (!(organ.containsKey("datumZapisu") && organ.containsKey("datumVymazu"))
                        && organ.containsKey("clenoveOrganu")) {
                    fillMembers(allOrgans, organ);
                }
            }
            icoDto.setCompanionNames(StringUtils.join(allOrgans, "; "));
        }
    }

    private void fillMembers(List<String> allOrgans, Map<String, Object> organ) {
        List<Map<String, Object>> memberList = (List<Map<String, Object>>) organ.get("clenoveOrganu");
        List<String> allMembers = new ArrayList<>();
        for (Map<String, Object> member : memberList) {
            if (!(member.containsKey("datumZapisu") && member.containsKey("datumVymazu"))) {
                if (member.containsKey("prijmeni")) {
                    parseMemberName(member, allMembers);
                }
                if (member.containsKey("fyzickaOsoba")) {
                    parseMemberName((Map<String, Object>) member.get("fyzickaOsoba"), allMembers);
                }
                if (member.containsKey("pravnickaOsoba")) {
                    fillLegalEntity(allMembers, member);
                }
            }
        }
        StringBuilder organMembers = new StringBuilder();
        if (organ.containsKey("nazevAngazma")) {
            organMembers.append(organ.get("nazevAngazma")).append(": ");
        }
        organMembers.append(StringUtils.join(allMembers, ", "));
        allOrgans.add(organMembers.toString());
    }

    private void fillLegalEntity(List<String> allMembers, Map<String, Object> member) {
        Map<String, Object> legalEntity = (Map<String, Object>) member.get("pravnickaOsoba");
        if (legalEntity.containsKey("zastoupeni")) {
            List<Map<String, Object>> representativeList = (List<Map<String, Object>>) legalEntity.get("zastoupeni");
            for (Map<String, Object> representative : representativeList) {
                if (!(representative.containsKey("datumZapisu") && representative.containsKey("datumVymazu"))
                        && representative.containsKey("fyzickaOsoba")) {
                    parseMemberName((Map<String, Object>) representative.get("fyzickaOsoba"), allMembers);
                }
            }
        }
    }

    private void fillCompanyName(Map<String, Object> vrSubject, IcoDto icoDto) {
        if (vrSubject.containsKey("obchodniJmeno")) {
            List<Map<String, Object>> companyNameList = (List<Map<String, Object>>) vrSubject.get("obchodniJmeno");
            for (Map<String, Object> companyName : companyNameList) {
                if (!(companyName.containsKey("datumZapisu") && companyName.containsKey("datumVymazu"))
                        && companyName.containsKey("hodnota")) {
                    icoDto.setCompanyName((String) companyName.get("hodnota"));
                }
            }
        }
    }

}
