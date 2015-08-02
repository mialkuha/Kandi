///Author: Mikko Kuha (31.7.2015)
//Laadittu kandidaatintyötäni "Ääretön potentiaalikuoppa liikkuvalla seinällä"
//varten käyttäen vain c++11 standardikirjastoja. Oma järjestelmäni on Cygwin-Unix,
//jota ajan Windows 8.1 päällä, millään muulla en takaa toimivuutta, mutta pitäisi kääntyä
//millä tahansa c++11 tukevalla kääntäjällä, sillä ei sisällä mitään laitteistokohtaisia funktioita.
//Ohjelma on tarkoitettu vain ja ainoastaan yksiulotteisen äärettömän potentiaalikuopan,
//jossa on vakionopeudella liikkuva seinä, tietokonesimulointiin.
//Se ei sovellu ilman raskaita muutoksia mihinkään muuhun numeriikkaan. Käytöohjeet
//löytyvät tiedostosta info.txt (TODO). Vaatii toimiakseen parametritiedoston params.dat

#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
#include <complex>
#include <vector>
#include <stdexcept>
#include <string>
#include <sstream>
#include <stdlib.h>

using namespace std;

//Määritellään vakioksi pii
constexpr double pi() { return atan(1)*4; }
//Määritellään vakioksi imaginaariyksikkö
constexpr complex<double> I() { return complex<double>(0.0,1.0); }

//Cygwinissä on bugi, jota varten Cygwin-käyttäjän on määriteltävä
//osa string-käsittelijöistä itse
namespace patch {
    int stoi(const string& n)
    {
        istringstream stm(n);
        int value;
        stm >> value;
        return value;
    }
    double stod(const string& n)
    {
        double value = strtod(n.c_str(), NULL);
        return value;
    }
}

//Matriisikirjasto aaltofunktioiden arvojen säilyttämiseen ja tallentamiseen
class Mat {
private:
    //Matriisin koko
    pair<int,int> size;
public:
    //Muuttuja, jossa kaikki matriisin data
    vector<vector<complex<double> > > mat;
    //Konstruktori. Luo spaces*times -matriisin kompleksisia nollia
    Mat(const int times, const int spaces){
                                size = make_pair(times,spaces);
                                vector<complex<double> > c(spaces,complex<double>(0.0,0.0));
                                vector<vector<complex<double> > > m(times,c);
                                mat = m;}

    pair<int,int> getSize() {
                                return size;};

    void print() {
                                for(int i=0; i<size.second; i++){
                                for(int j=0; j<size.first; j++){
                                cout<<mat[j][i]<<' ';
                                }
                                cout<<endl;};};

    //Tallettaa pisteittäisen todennäköisyyden
    void saveProb(const string filename, const int jump) {
                                ofstream file;
                                file.open (filename);
                                for(int i=0; i<size.second; i++){
                                    for(int j=0; j<size.first; j+=jump){
                                        file <<norm(mat[j][i])<<' ';
                                    }
                                    file <<'\n';
                                }
                                file.close();};

    //Tallettaa koko matriisin kompleksimatriisina
    void save(const string filename, const int jump) {
                                ofstream file;
                                file.open (filename);
                                for(int i=0; i<size.second; i++){
                                    for(int j=0; j<size.first; j+=jump){
                                        file <<mat[j][i]<<' ';
                                    }
                                    file <<'\n';
                                }
                                file.close();};

    //Tallettaa pisteittäiset reaali- ja imaginaariosat kahteen matriisiin
    void save(const string realname, const string imagname, const int jump) {
                                ofstream file;
                                file.open (realname);
                                for(int i=0; i<size.second; i++){
                                    for(int j=0; j<size.first; j+=jump){
                                        file <<real(mat[j][i])<<' ';
                                    }
                                    file <<'\n';
                                }
                                file.close();
                                file.open (imagname);
                                for(int i=0; i<size.second; i++){
                                    for(int j=0; j<size.first; j+=jump){
                                        file <<imag(mat[j][i])<<' ';
                                    }
                                    file <<'\n';
                                }
                                file.close();};

    //Tallettaa pisteittäiset normit ja argumentit kahteen matriisiin
    void saveNA(const string normname, const string argname, const int jump) {

                                ofstream file;
                                file.open (normname);
                                for(int i=0; i<size.second; i++){
                                    for(int j=0; j<size.first; j+=jump){
                                        file <<abs(mat[j][i])<<' ';
                                    }
                                    file <<'\n';
                                }
                                file.close();
                                file.open (argname);
                                for(int i=0; i<size.second; i++){
                                    for(int j=0; j<size.first; j+=jump){
                                        file <<arg(mat[j][i])<<' ';
                                    }
                                    file <<'\n';
                                }
                                file.close();};

    //palauttaa tämän ja parametrimatriisin välisen erotuksen matriisina
    Mat error(const Mat A)            {
                                Mat err(size.first,size.second);
                                for(int i=0;i<size.first;i++){
                                    for(int j=0;j<size.second;j++){
                                            err.mat[i][j] = mat[i][j] - A.mat[i][j];
                                    }
                                }
                                return err;};
};

//Luokka ohjelman parametrien lukuun
class Params {
private:
public:
    int xi=100, xn=200, TE=500, ti=0, tn=500, n=4, nIntegralGrid=100, integralDeg=3, nAnalyticBaseFs=20;
    double k=2.5;
    int jump=1;
    bool analyt=true, adiabat=true, ftime=false, btime=false, ctime=false, errorAN=false, errorAD=false, errorDN=false;
    bool prob=true, reim=true, normarg=false;
    Params(){
        ifstream params;
        params.open("params.dat");
        if (params.is_open()){
            string keys[22][2];
            int j=0;
            string line;
            while(getline(params, line)){
                istringstream is_line(line);
                string key;
                if(getline(is_line, key, '=') ){
                    string value;
                    if(getline(is_line, value, ' ')){
                        keys[j][0] = key;
                        keys[j][1] = value;
                        j++;
                    }
                }
            }
            for(int i=0; i<j; i++){
                if (keys[i][0]=="initial_width") xi = patch::stoi(keys[i][1]);
                else if (keys[i][0]=="spatial_pts") xn = patch::stoi(keys[i][1]);
                else if (keys[i][0]=="external_time") TE = patch::stoi(keys[i][1]);
                else if (keys[i][0]=="initial_time") ti = patch::stoi(keys[i][1]);
                else if (keys[i][0]=="time_pts") tn = patch::stoi(keys[i][1]);
                else if (keys[i][0]=="initial_degree") n = patch::stoi(keys[i][1]);
                else if (keys[i][0]=="grid_time_dnst") k = patch::stod(keys[i][1]);

                else if (keys[i][0]=="integral_grid_pts") nIntegralGrid = patch::stoi(keys[i][1]);
                else if (keys[i][0]=="integral_degree") integralDeg = patch::stoi(keys[i][1]);
                else if (keys[i][0]=="base_funcs") nAnalyticBaseFs = patch::stoi(keys[i][1]);

                else if (keys[i][0]=="data_jump") jump = patch::stoi(keys[i][1]);
                else if (keys[i][0]=="save_probabilities" && keys[i][1]=="1") prob = true;
                else if (keys[i][0]=="save_real_imag" && keys[i][1]=="1") reim = true;
                else if (keys[i][0]=="save_norm_arg" && keys[i][1]=="1") normarg = true;
                else if (keys[i][0]=="calc_analytic" && keys[i][1]=="1") analyt = true;
                else if (keys[i][0]=="calc_adiabat" && keys[i][1]=="1") adiabat = true;
                else if (keys[i][0]=="calc_forwardtime" && keys[i][1]=="1") ftime = true;
                else if (keys[i][0]=="calc_backwardtime" && keys[i][1]=="1") btime = true;
                else if (keys[i][0]=="calc_centraltime" && keys[i][1]=="1") ctime = true;
                else if (keys[i][0]=="ecalc_analytic_numeric" && keys[i][1]=="1") errorAN = true;
                else if (keys[i][0]=="ecalc_analytic_adiabat" && keys[i][1]=="1") errorAD = true;
                else if (keys[i][0]=="ecalc_adiabat_numeric" && keys[i][1]=="1") errorDN = true;
            }
        }
        else {
            cout<<"params.dat not available"<<endl;
            throw runtime_error("params.dat not available");
        }
    }
};


//Eksplisiittinen Eulerin menetelmä. Toimii harvoin
void fwardTime1stO(const vector<complex<double> >& psi0, Mat& psi, const double k, const double h, const int ti, const int tn, const double vInt, const int xi){

    complex<double> r = complex<double>(0.0,k/pow(h,2));
    psi.mat[0] = psi0;

    for(int i=1;i<tn;i++){
        int jmax = (int)((xi+(i+ti)*vInt)/h);
        psi.mat[i][0] = complex<double>(0.0,0.0); //reunaehto

        for(int j=1;j<jmax-1;j++){
            psi.mat[i][j] = r*psi.mat[i-1][j-1]
                            + (1.0-2.0*r)*psi.mat[i-1][j]
                            + r*psi.mat[i-1][j+1];
        }
        psi.mat[i][jmax-1]= r*psi.mat[i-1][jmax-3]
                            + (1.0-2.0*r)*psi.mat[i-1][jmax-2]
                            + r*psi.mat[i-1][jmax-1];

        psi.mat[i][jmax] = complex<double>(0.0,0.0); //reunaehto
    }

    return;
}

//Apufunktio implisiittiseen Eulerin menetelmään
void initializeCoeffsForIE(vector<complex<double> >& coeffs, const complex<double> r){
    coeffs[0] = 1.0+2.0*r;
    coeffs[1] = -r;
    coeffs[2] = complex<double>(0.0,0.0);
    coeffs[3] = complex<double>(0.0,0.0);
    coeffs[4] = -r;
    coeffs[5] = 1.0+2.0*r;
    coeffs[6] = coeffs[4];
    coeffs[7] = complex<double>(0.0,0.0);
    for(int i=8;i<coeffs.size()-4;i+=3){
        coeffs[i] = coeffs[4];
        coeffs[i+1] = coeffs[5];
        coeffs[i+2] = coeffs[4];
    }
    coeffs[coeffs.size()-2] = coeffs[4];
    coeffs[coeffs.size()-1] = coeffs[5];

    for(int k=1;k<4;k++){
        coeffs[k] = coeffs[k]/coeffs[0];
    }
    coeffs[5] = coeffs[5]-coeffs[1]*coeffs[4];
    for(int k=6;k<8;k++){
            coeffs[k] = (coeffs[k]-coeffs[k-4]*coeffs[4])/coeffs[5];
    }
    coeffs[9] = coeffs[9]-coeffs[6]*coeffs[8];
    coeffs[10] = (coeffs[10]-coeffs[7]*coeffs[8])/coeffs[9];
    for(int k=11;k<coeffs.size()-4;k+=3){
            coeffs[k+1] = coeffs[k+1]-coeffs[k-1]*coeffs[k];
            coeffs[k+2] = coeffs[k+2]/coeffs[k+1];
    }
    coeffs[coeffs.size()-1] = coeffs[coeffs.size()-1]-coeffs[coeffs.size()-3]*coeffs[coeffs.size()-2];
}

//Implisiittinen Eulerin menetelmä. Toimii harvoin, mutta useammin kuin eksplisiittinen
void bwardTime1stO(const vector<complex<double> >& psi0, Mat& psi, const double k, const double h, const int ti, const int tn, const double vInt, const int xi){

    complex<double> r = complex<double>(0.0,k/pow(h,2));
    psi.mat[0] = psi0;

    vector<complex<double> > coeffs(3*psi0.size()+1);
    initializeCoeffsForIE(coeffs, r);
    vector<complex<double> > psidot;

    for(int i=1;i<tn;i++){
        int jmax = (int)((xi+(i+ti)*vInt)/h);
        psidot = psi.mat[i-1];

        psidot[0] = psidot[0]/coeffs[0];
        psidot[1] = (psidot[1]-coeffs[4]*psidot[0])/coeffs[5];
        for(int j=2;j<jmax;j++){
                psidot[j] = (psidot[j]-coeffs[3*(j+1)-1]*psidot[j-1])/coeffs[3*(j+1)];
        }

        psi.mat[i][jmax] = complex<double>(0.0,0.0); //reunaehto
        for(int j=jmax-1;j>1;j--){
                psi.mat[i][j] = psidot[j]-coeffs[3*j+4]*psi.mat[i][j+1];
        }
        psi.mat[i][1] = psidot[1]-coeffs[6]*psi.mat[i][2]-coeffs[7]*psi.mat[i][3];
        psi.mat[i][0] = complex<double>(0.0,0.0); //reunaehto
    }

    return;
}

//Apufunktio Crankin ja Nicolsonin menetelmälle
void initializeCoeffsForCN(vector<complex<double> >& coeffs, const complex<double> r){
    coeffs[0] = 2.0+2.0*r;
    coeffs[1] = -r;
    coeffs[2] = complex<double>(0.0,0.0);
    coeffs[3] = complex<double>(0.0,0.0);
    coeffs[4] = -r;
    coeffs[5] = 2.0+2.0*r;
    coeffs[6] = coeffs[4];
    coeffs[7] = complex<double>(0.0,0.0);
    for(int i=8;i<coeffs.size()-4;i+=3){
        coeffs[i] = coeffs[4];
        coeffs[i+1] = coeffs[5];
        coeffs[i+2] = coeffs[4];
    }
    coeffs[coeffs.size()-2] = coeffs[4];
    coeffs[coeffs.size()-1] = coeffs[5];

    for(int k=1;k<4;k++){
        coeffs[k] = coeffs[k]/coeffs[0];
    }
    coeffs[5] = coeffs[5]-coeffs[1]*coeffs[4];
    for(int k=6;k<8;k++){
            coeffs[k] = (coeffs[k]-coeffs[k-4]*coeffs[4])/coeffs[5];
    }
    coeffs[9] = coeffs[9]-coeffs[6]*coeffs[8];
    coeffs[10] = (coeffs[10]-coeffs[7]*coeffs[8])/coeffs[9];
    for(int k=11;k<coeffs.size()-4;k+=3){
            coeffs[k+1] = coeffs[k+1]-coeffs[k-1]*coeffs[k];
            coeffs[k+2] = coeffs[k+2]/coeffs[k+1];
    }
    coeffs[coeffs.size()-1] = coeffs[coeffs.size()-1]-coeffs[coeffs.size()-3]*coeffs[coeffs.size()-2];
}

//Crankin ja Nicolsonin menetelmä. Vaatii tiheän hilan ja pienen määrän aika-askeleita
void ctralTime2ndO(const vector<complex<double> >& psi0, Mat& psi, const double k, const double h, const int ti, const int tn, const double vInt, const int xi){

    complex<double> r = complex<double>(0.0,k/pow(h,2));
    psi.mat[0] = psi0;

    vector<complex<double> > coeffs(3*psi0.size()+1);
    initializeCoeffsForCN(coeffs, r);
    vector<complex<double> > psidot;

    for(int i=1;i<tn;i++){
        int jmax = (int)((xi+(i+ti)*vInt)/h);

        psi.mat[i][0] = complex<double>(0.0,0.0); //reunaehto
        for(int j=1;j<jmax-1;j++){
            psi.mat[i][j] = r*psi.mat[i-1][j-1]
                            + (2.0-2.0*r)*psi.mat[i-1][j]
                            + r*psi.mat[i-1][j+1];
        }
        psi.mat[i][jmax-1]= r*psi.mat[i-1][jmax-3]
                            + (2.0-2.0*r)*psi.mat[i-1][jmax-2]
                            + r*psi.mat[i-1][jmax-1];

        psi.mat[i][jmax] = complex<double>(0.0,0.0); //reunaehto


        psidot = psi.mat[i];

        psidot[0] = psidot[0]/coeffs[0];
        psidot[1] = (psidot[1]-coeffs[4]*psidot[0])/coeffs[5];
        for(int j=2;j<jmax;j++){
                psidot[j] = (psidot[j]-coeffs[3*(j+1)-1]*psidot[j-1])/coeffs[3*(j+1)];
        }

        psi.mat[i][jmax] = complex<double>(0.0,0.0); //reunaehto
        for(int j=jmax-1;j>1;j--){
                psi.mat[i][j] = psidot[j]-coeffs[3*j+4]*psi.mat[i][j+1];
        }
        psi.mat[i][1] = psidot[1]-coeffs[6]*psi.mat[i][2]-coeffs[7]*psi.mat[i][3];
        psi.mat[i][0] = complex<double>(0.0,0.0); //reunaehto
    }

    return;
}

//Apufunktio, joka laskee analyyttisten kantafunktioiden lineaarikertoimet
void coeffs(vector<complex<double> >& c, const int nGrid, const int nI, const int maxNEnergia, const double a, const double v, const int deg =2){

    double h = pi()/nGrid;
    double alpha = 0.25*v*a/(pi()*pi());
    double quad[nGrid+1];

    switch (deg){ //laaditaan kvadratuuri
    case 1:
        quad[0] = 0.5; //trapetsoidikvadratuuri
        for(int i=1; i<nGrid; i++) quad[i] = 1;
        quad[nGrid] = 0.5;
        break;
    case 3:
        quad[0] = 3/8; //simpsonin 3/8 säännön kvadratuuri
        for(int i=1; i<nGrid; i+=3){
                quad[i] = 9/8;
                quad[i+1] = 9/8;
                quad[i+2] = 6/8;
        }
        quad[nGrid] = 3/8;
        break;
    default:
        quad[0] = 1/3; //simpsonin säännön kvadratuuri (2.- ja 3. aste)
        for(int i=1; i<nGrid; i+=2){
                quad[i] = 4/3;
                quad[i+1] = 2/3;
        }
        quad[nGrid] = 1/3;
        break;
    }

    ofstream file;
    file.open ("cs.dat");

    for(int n=0; n<maxNEnergia; n++){
        c[n] = complex<double>(0.0,0.0);

        for(int i=0; i<=nGrid; i++) c[n] += 2*h*quad[i]*sin((n+1)*i*h)*sin(nI*i*h)
                                            *exp(complex<double>(0.0,-alpha*pow(i*h,2)))
                                            /pi(); //10.5
        file <<c[n]<<' '<<abs(c[n])<<'\n';
    }

    file.close();
}


int main(){
    const Params params;

    const int xi = params.xi; //alkuleveys
    const int xE = 2*xi; //loppuleveys
    const int xn = params.xn; //paikka-askeleet
    const double h = (double)xE/xn; //verkon paikkatiheys

    const int TE = params.TE; //aika-askel, jolla kaivo tuplaantuu
    const int ti = params.ti; //tarkasteltavan ajan ensimmäinen askel
    const int tn = params.tn; //aika-askeleet
    const int te = ti+tn; //tarkasteltavan ajan viimeinen askel
    const double k = params.k; //verkon aikatiheys

    const int n = params.n; //alkutilan kertaluku
    const double v = (double)(h*xi)/(k*TE); //levenemisnopeus
    const double vInt = (double)xi/TE; //levenemisnopeus indekseissä
    const double a = xi*h; //alkutilan leveys
    const double ai = xi*h +ti*k*v; //tarkasteluvälin alkutilan leveys
    const double eI = n*n*pi()*pi()/(a*a); //alkutilan energia

    Mat analytPsi(tn,xn);
    Mat adiabatPsi(tn,xn);
    Mat numericPsi(tn,xn);


    //Analyyttisten kantafunktioiden mukaan lasketut tulokset
    if(params.analyt){
        const int nIntegralGrid = params.nIntegralGrid; //integrointivälien määrä
        const int integralDeg = params.integralDeg; //käytettävän kvadratuurin kertaluku
        const int nAnalyticBaseFs = params.nAnalyticBaseFs; //analyyttisten kantafunktioiden määrä
        double eIn[nAnalyticBaseFs];//alkutilan energia eri energiatiloissa
        for(int m=0; m<nAnalyticBaseFs; m++){
            eIn[m] += (m+1)*(m+1)*pi()*pi()/(a*a);
        }

        vector<complex<double> > c(nAnalyticBaseFs,complex<double>(0.0,0.0));
        coeffs(c, nIntegralGrid, n, nAnalyticBaseFs, a, v, integralDeg);

        for(int i=0;i<tn;i++){
            for(int j=0;j<(int)((xi+(i+ti)*vInt)/h);j++){
                const double x = h*h*j;
                const double t = k*(i+ti);
                const double w = a+v*t;

                complex<double> dummy = complex<double>(0.0,0.0);

                for(int m=0; m<nAnalyticBaseFs; m++){
                    dummy += c[m]*sqrt(2.0/w)*sin((m+1)*pi()*x/w)
                            *exp(complex<double>(0.0,(0.5*v*pow(x,2)-2.0*eIn[m]*a*t)/(2.0*w)));
                }

                analytPsi.mat[i][j] = dummy;
            }
        }
        double sum=0;
        for(auto elem:c)sum+=norm(elem);
        cout<<"sum="<<sum<<endl;
        for(int m=0; m<nAnalyticBaseFs; m++) cout<<"c"<<(m+1)<<"="<<c[m]<<endl;
    }


    //Adiabaattisen approksimaation mukainen tulos
    if(params.adiabat){
        for(int i=0;i<tn;i++){
            for(int j=0;j<(int)((xi+(i+ti)*vInt)/h);j++){
                const double x = h*h*j;
                const double t = k*(i+ti);
                const double w = a+v*t;
                const double theta = -n*n*pi()*pi()*t/(a*w); //integroitu vaihetekijä 10.6
                adiabatPsi.mat[i][j] = sqrt(2.0/w)*sin(n*pi()*x/w)
                                        *exp(complex<double>(0.0,theta));
            }
        }
    }


    //Numeeriset tulokset
    const bool numeric = (params.btime || params.ftime || params.ctime);
    if(numeric){
        vector<complex<double> > psi0(xn);
        for(int i=0; i<(int)((xi+ti*vInt)/h); i++) psi0[i] = sqrt(2.0/a)*sin(n*pi()*i*h*h/a);
        for(int i=(int)((xi+ti*vInt)/h); i<xE; i++) psi0[i] = 0;

        if(params.ctime) ctralTime2ndO(psi0, numericPsi, k, h, ti, tn, vInt, xi);
        else if(params.btime) fwardTime1stO(psi0, numericPsi, k, h, ti, tn, vInt, xi);
        else if(params.ftime) fwardTime1stO(psi0, numericPsi, k, h, ti, tn, vInt, xi);
    }


    //Tallennusrutiinit
    {
        if(params.prob){
            if(params.analyt) analytPsi.saveProb("analytic_psi_probability.dat", params.jump);
            if(params.adiabat) adiabatPsi.saveProb("adiabatic_psi_probability.dat", params.jump);
            if(numeric) numericPsi.saveProb("numeric_psi_probability.dat", params.jump);
        }

        if(params.reim){
            if(params.analyt) analytPsi.save("analytic_psi_real.dat","analytic_psi_imag.dat", params.jump);
            if(params.adiabat) adiabatPsi.save("adiabatic_psi_real.dat","adiabatic_psi_imag.dat", params.jump);
            if(numeric) numericPsi.save("numeric_psi_real.dat","numeric_psi_imag.dat", params.jump);
        }

        if(params.normarg){
            if(params.analyt) analytPsi.saveNA("analytic_psi_norm.dat","analytic_psi_argument.dat", params.jump);
            if(params.adiabat) adiabatPsi.saveNA("adiabatic_psi_norm.dat","adiabatic_psi_argument.dat", params.jump);
            if(numeric) numericPsi.saveNA("numeric_psi_norm.dat","numeric_psi_argument.dat", params.jump);
        }

        if(params.errorAD && params.analyt && params.adiabat){
            Mat error = analytPsi.error(adiabatPsi);
            if(params.prob) error.saveProb("an-ad_error_probability", params.jump);
            if(params.reim) error.save("an-ad_error_real.dat","an-ad_error_imag.dat", params.jump);
            if(params.normarg) error.saveNA("an-ad_error_norm.dat","an-ad_error_argument.dat", params.jump);
        }

        if(params.errorAN && params.analyt && numeric){
            Mat error = analytPsi.error(numericPsi);
            if(params.prob) error.saveProb("an-nu_error_probability", params.jump);
            if(params.reim) error.save("an-nu_error_real.dat","an-nu_error_imag.dat", params.jump);
            if(params.normarg) error.saveNA("an-nu_error_norm.dat","an-nu_error_argument.dat", params.jump);
        }

        if(params.errorDN && params.adiabat && numeric){
            Mat error = adiabatPsi.error(numericPsi);
            if(params.prob) error.saveProb("ad-nu_error_probability", params.jump);
            if(params.reim) error.save("ad-nu_error_real.dat","ad-nu_error_imag.dat", params.jump);
            if(params.normarg) error.saveNA("ad-nu_error_norm.dat","ad-nu_error_argument.dat", params.jump);
        }
    }


    return 1;
}
